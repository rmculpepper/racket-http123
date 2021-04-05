#lang racket/base
(require racket/match
         racket/vector
         binaryio/reader
         "regexp.rkt"
         "bitvector.rkt")
(provide (all-defined-out))

;; Reference:
;; - https://tools.ietf.org/html/rfc7541

;; Note on normalization: RFC 7541 says header names are treated as
;; opaque sequences of octets. But HTTP/2 .... (FIXME)

;; ----------------------------------------

;; A NormalizedHeader is one of
;; - (list Bytes Bytes)
;; - (list Bytes Bytes 'never-add)

;; A FlexibleHeader is one of
;; - Bytes
;; - (list FlexibleKey FlexibleValue)
;; - (list FlexibleKey FlexibleValue 'never-add)

;; A FlexibleKey is one of
;; - Symbol
;; - Bytes      -- normalize ???

;; A FlexibleValue is one of
;; - Bytes          -- normal header or unsplit list-valued header
;; - (Listof Bytes) -- list-valued header

;; encode-headers : OutputPort (Listof FlexibleHeader) State -> Void
(define (encode-headers out headers dt #:who [who 'encode-headers])
  (define header-reps (encode-headers* headers dt #:who who))
  (for ([hrep (in-list header-reps)])
    (write-header-rep out hrep)))

(define (encode-headers* headers dt #:who [who 'encode-headers*])
  (for/list ([header (in-list headers)])
    (encode-header who header dt)))

(define (encode-header who header dt)
  (match header
    [(list key value 'never-add)
     (define key* (normalize-key who key))
     (header:literal 'never-add key* value)]
    [(list key value)
     (define key* (normalize-key who key))
     (define key-index (or (hash-ref key-table key* #f)
                           (dtable-find-key dt key* (dtable-adjustment))))
     (cond [(hash-ref key+value-table (list key* value) #f)
            => (lambda (index) (header:indexed index))]
           [(dtable-find dt (list key* value) (dtable-adjustment))
            => (lambda (index) (header:indexed index))]
           [(member key* keys-to-index-headers)
            (dtable-add! dt (list key* value))
            (header:literal 'add (or key-index key*) value)]
           [key-index
            (header:literal 'no-add key-index value)]
           [else
            (header:literal 'no-add key* value)])]))

(define (normalize-key who key0)
  (let loop ([key key0])
    (cond [(bytes? key) key]
          [(symbol? key) (loop (symbol->string key))]
          [(and (string? key) (regexp-match? (rx^$ (rx ":?" TOKEN)) key))
           (string->bytes/latin-1 key)]
          [else (error who "bad header key\n  key: ~e" key0)])))

(define keys-to-index-headers
  '(#":authority"
    ;; #":path" ;; maybe?
    #"accept-charset"
    #"accept-language"
    #"authorization"
    #"cookie"
    #"content-type"
    #"host"
    #"proxy-authorization"
    #"referer"
    #"transfer-encoding"
    #"user-agent"))

;; decode-headers : Bytes DTable -> (Listof NormalizedHeader)
(define (decode-headers bs dt)
  (define br (make-binary-reader (open-input-bytes bs)))
  (define header-reps (read-header-reps br))
  (for/list ([hrep (in-list header-reps)])
    (decode-header hrep dt)))

(define (read-header-reps br)
  (let loop ()
    (cond [(b-at-limit? br) null]
          [else (cons (read-header-rep br) (loop))])))

(define (decode-header h dt)
  (match h
    [(header:indexed index)
     (table-ref dt index)]
    [(header:literal mode key value)
     (define e
       (list* (if (bytes? key) key (car (table-ref dt key)))
              value
              (case mode [(never-add) '(never-add)] [else '()])))
     (when (eq? mode 'add) (dtable-add! dt e))
     e]
    [(header:maxsize new-maxsize)
     (set-dtable-maxsize! dt new-maxsize)
     (dtable-check-evict dt)]))

;; ------------------------------------------------------------

;; A DTable is a ring buffer that stores virtual size information.
(struct dtable
  (vec          ;; vector storing entries
   start        ;; index of oldest entry
   count        ;; number of entries
   maxsize      ;; maximum size
   size         ;; current size
   ) #:mutable)

(define DTABLE-INIT-CAPACITY 16)
(define DTABLE-INIT-MAXSIZE 4096)

(define (make-dtable max-size)
  (dtable (make-vector DTABLE-INIT-CAPACITY) 0 0 max-size 0))

(define (dtable-copy dt)
  (match-define (dtable vec start count maxsize size) dt)
  (dtable (vector-copy vec) start count maxsize size))

(define (dtable-ref dt index)
  (vector-ref (dtable-vec dt) (dtable:index->vector-index 'dtable-ref dt index)))

(define (dtable:index->vector-index who dt index)
  (match-define (dtable vec start count _ms _s) dt)
  (unless (< index count) (error who "index out of range\n  index: ~e" index))
  (remainder (+ start count -1 (- index))
             (vector-length vec)))

;; dtable-adjustment : -> Nat, add to dtable index to get (global) index
(define (dtable-adjustment) (add1 static-table-length))

(define (dtable-find dt entry [adjust 0])
  (for/or ([index (in-range (dtable-count dt))])
    (define e (dtable-ref dt index))
    (and (equal? e entry) (+ index adjust))))

(define (dtable-find-key dt key [adjust 0])
  (for/or ([index (in-range (dtable-count dt))])
    (define e (dtable-ref dt index))
    (and (equal? (car e) key) (+ index adjust))))

(define (dtable-add! dt entry)
  (match-define (dtable vec start count _ms old-size) dt)
  (cond [(< count (vector-length vec))
         (vector-set! vec (remainder (+ start count) (vector-length vec)) entry)
         (set-dtable-size! dt (+ old-size (entry-size entry)))
         (dtable-check-evict dt)]
        [else
         (define new-vec (make-vector (* 3 (quotient (vector-length vec) 2))))
         (vector-copy! new-vec 0 vec start (vector-length vec))
         (vector-copy! new-vec (vector-length vec) vec 0 (max 0 (- (vector-length vec) count)))
         (set-dtable-vec! dt new-vec)
         (set-dtable-start! dt 0)
         (dtable-add! dt entry)]))

(define (dtable-check-evict dt)
  (match-define (dtable vec start count maxsize size) dt)
  (unless (<= size maxsize)
    (let loop ([start start] [count count] [size size])
      (cond [(<= size maxsize)
             (set-dtable-start! dt start)
             (set-dtable-count! dt count)
             (set-dtable-size!  dt size)]
            [else
             (define esize (entry-size (vector-ref vec start)))
             (vector-set! vec start #f)
             (loop (modulo (add1 start) (vector-length vec))
                   (sub1 count)
                   (- size esize))]))))

(define (entry-size entry)
  (+ 32
     (bytes-length (car entry))
     (and (cadr entry) (bytes-length (cadr entry)))))

;; ------------------------------------------------------------
;; 5.1 Integer representation

;; read-intrep : BinaryReader Nat[1,8] Byte -> Nat
(define (read-intrep br prefix b)
  (let ([b (bitwise-bit-field b 0 prefix)])
    (define all-ones (sub1 (arithmetic-shift 1 prefix)))
    (cond [(= b all-ones)
           (+ all-ones (read-intrep-k br))]
          [else b])))
(define (read-intrep-k br)
  (let loop ([acc 0] [power 0])
    (define b (b-read-byte br))
    (define acc* (+ acc (arithmetic-shift (bitwise-bit-field b 0 6) power)))
    (if (bitwise-bit-set? b 7) (loop acc* (+ power 7)) acc*)))

(define (write-intrep out prefix high-b n)
  (define b (arithmetic-shift high-b prefix))
  (define all-ones (sub1 (arithmetic-shift 1 prefix)))
  (cond [(< n all-ones)
         (write-byte (+ b n) out)]
        [else
         (write-byte (+ b all-ones) out)
         (write-intrep-k out (- n all-ones))]))
(define (write-intrep-k out n)
  (cond [(< n #x80)
         (write-byte n out)]
        [else
         (write-byte (+ #x80 (bitwise-bit-field n 0 7)) out)
         (write-intrep-k out (arithmetic-shift n -7))]))

;; 5.2 String Literal Representation

(define (read-string-literal br)
  (define b (b-read-byte br))
  (define huffman? (bitwise-bit-set? b 7))
  (define len (read-intrep br 7 b))
  (define bs (b-read-bytes br len))
  (if huffman? (h-decode bs) bs))

(define H-ENC-MIN-LEN 0)        ;; FIXME?
(define H-ENC-MAX-LEN +inf.0)   ;; FIXME?

(define (write-string-literal out bs)
  (define enc (and (<= H-ENC-MIN-LEN (bytes-length bs) H-ENC-MAX-LEN)
                   (h-encode bs)))
  (cond [(and enc (< (bytes-length enc) (bytes-length bs)))
         (write-intrep out 7 #b1 (bytes-length enc))
         (write-bytes enc out)]
        [else
         (write-intrep out 7 #b0 (bytes-length bs))
         (write-bytes bs out)]))

;; 6 Binary Format

(struct header:indexed (index) #:prefab)
(struct header:literal (mode key value) #:prefab)
(struct header:maxsize (maxsize) #:prefab)

(define (read-header-rep br)
  (define b (b-read-byte br))
  (cond [(bitwise-bit-set? b 7) ;; #b1...
         ;; 6.1 Indexed Header Field Representation
         (define index (read-intrep br 7 b))
         (when (zero? index)
           (error 'read-header-rep "illegal zero index"))
         (header:indexed index)]
        [(bitwise-bit-set? b 6) ;; #b01...
         ;; 6.2.1 Literal Header Field with Incremental Indexing
         (define index (read-intrep br 6 b))
         (define key
           (cond [(zero? index) (read-string-literal br)]
                 [else index]))
         (define value (read-string-literal br))
         (header:literal 'add key value)]
        [(= #b000 (bitwise-bit-field b 5 8)) ;; #b000...
         ;; 6.2.{2,3} Literal Header Field {without Indexing, Never Indexed}
         (define mode (if (bitwise-bit-set? b 4) 'never-add 'no-add))
         (define index (read-intrep br 4 b))
         (define key
           (cond [(zero? index) (read-string-literal br)]
                 [else index]))
         (define value (read-string-literal br))
         (header:literal mode key value)]
        [(= #b001 (bitwise-bit-field b 5 8)) ;; #b001...
         (define new-maxsize (read-intrep br 5 b))
         (header:maxsize new-maxsize)]))

(define (write-header-rep out h)
  (match h
    [(header:indexed index)
     (write-intrep out 7 #b1 index)]
    [(header:literal mode key value)
     (define keyindex (if (bytes? key) 0 key))
     (case mode
       [(add) (write-intrep out 6 #b01 keyindex)]
       [(no-add) (write-intrep out 4 #b0000 keyindex)]
       [(never-add) (write-intrep out 4 #b0001 keyindex)])
     (when (bytes? key) (write-string-literal out key))
     (write-string-literal out value)]
    [(header:maxsize new-maxsize)
     (write-intrep out 5 #b001 new-maxsize)]))

;; ------------------------------------------------------------

(define (table-ref dt index)
  (if (<= index static-table-length)
      (static-table-ref index)
      (dtable-ref dt (- index 1 static-table-length))))

;; ------------------------------------------------------------

(define static-table
  '#[;;(Index)   HeaderName                       HeaderValue
     [#;  1       #":authority"                   #""]
     [#;  2       #":method"                      #"GET"           ]
     [#;  3       #":method"                      #"POST"          ]
     [#;  4       #":path"                        #"/"             ]
     [#;  5       #":path"                        #"/index.html"   ]
     [#;  6       #":scheme"                      #"http"          ]
     [#;  7       #":scheme"                      #"https"         ]
     [#;  8       #":status"                      #"200"           ]
     [#;  9       #":status"                      #"204"           ]
     [#; 10       #":status"                      #"206"           ]
     [#; 11       #":status"                      #"304"           ]
     [#; 12       #":status"                      #"400"           ]
     [#; 13       #":status"                      #"404"           ]
     [#; 14       #":status"                      #"500"           ]
     [#; 15       #"accept-charset"               #""]
     [#; 16       #"accept-encoding"              #"gzip, deflate" ]
     [#; 17       #"accept-language"              #""]
     [#; 18       #"accept-ranges"                #""]
     [#; 19       #"accept"                       #""]
     [#; 20       #"access-control-allow-origin"  #""]
     [#; 21       #"age"                          #""]
     [#; 22       #"allow"                        #""]
     [#; 23       #"authorization"                #""]
     [#; 24       #"cache-control"                #""]
     [#; 25       #"content-disposition"          #""]
     [#; 26       #"content-encoding"             #""]
     [#; 27       #"content-language"             #""]
     [#; 28       #"content-length"               #""]
     [#; 29       #"content-location"             #""]
     [#; 30       #"content-range"                #""]
     [#; 31       #"content-type"                 #""]
     [#; 32       #"cookie"                       #""]
     [#; 33       #"date"                         #""]
     [#; 34       #"etag"                         #""]
     [#; 35       #"expect"                       #""]
     [#; 36       #"expires"                      #""]
     [#; 37       #"from"                         #""]
     [#; 38       #"host"                         #""]
     [#; 39       #"if-match"                     #""]
     [#; 40       #"if-modified-since"            #""]
     [#; 41       #"if-none-match"                #""]
     [#; 42       #"if-range"                     #""]
     [#; 43       #"if-unmodified-since"          #""]
     [#; 44       #"last-modified"                #""]
     [#; 45       #"link"                         #""]
     [#; 46       #"location"                     #""]
     [#; 47       #"max-forwards"                 #""]
     [#; 48       #"proxy-authenticate"           #""]
     [#; 49       #"proxy-authorization"          #""]
     [#; 50       #"range"                        #""]
     [#; 51       #"referer"                      #""]
     [#; 52       #"refresh"                      #""]
     [#; 53       #"retry-after"                  #""]
     [#; 54       #"server"                       #""]
     [#; 55       #"set-cookie"                   #""]
     [#; 56       #"strict-transport-security"    #""]
     [#; 57       #"transfer-encoding"            #""]
     [#; 58       #"user-agent"                   #""]
     [#; 59       #"vary"                         #""]
     [#; 60       #"via"                          #""]
     [#; 61       #"www-authenticate"             #""]])

(define static-table-length (vector-length static-table))

;; Note: indexed from 1
(define (static-table-ref n)
  (unless (<= 1 n (vector-length static-table))
    (error 'static-table-ref "out of range"))
  (vector-ref static-table (sub1 n)))

(define-values (key+value-table key-table)
  (for/fold ([kvh (hash)] [kh (hash)])
            ([kv (in-vector static-table)]
             [index (in-naturals 1)])
    (values (hash-set kvh kv index)
            (if (hash-has-key? kh (car kv)) kh (hash-set kh (car kv) index)))))

(define (key+value->static-index kv)
  (hash-ref key+value-table kv #f))

(define (key->static-index k)
  (hash-ref key-table k #f))

;; ------------------------------------------------------------

(define h-table-src
  '#[;;                    code as bits                   as hex     len
     ;;  sym              aligned to MSB                  aligned     in
     ;;                                                   to LSB     bits
     [#;   0  #;  _11111111_11000                             #x1ff8  13]
     [#;   1  #;  _11111111_11111111_1011000                #x7fffd8  23]
     [#;   2  #;  _11111111_11111111_11111110_0010         #xfffffe2  28]
     [#;   3  #;  _11111111_11111111_11111110_0011         #xfffffe3  28]
     [#;   4  #;  _11111111_11111111_11111110_0100         #xfffffe4  28]
     [#;   5  #;  _11111111_11111111_11111110_0101         #xfffffe5  28]
     [#;   6  #;  _11111111_11111111_11111110_0110         #xfffffe6  28]
     [#;   7  #;  _11111111_11111111_11111110_0111         #xfffffe7  28]
     [#;   8  #;  _11111111_11111111_11111110_1000         #xfffffe8  28]
     [#;   9  #;  _11111111_11111111_11101010               #xffffea  24]
     [#;  10  #;  _11111111_11111111_11111111_111100      #x3ffffffc  30]
     [#;  11  #;  _11111111_11111111_11111110_1001         #xfffffe9  28]
     [#;  12  #;  _11111111_11111111_11111110_1010         #xfffffea  28]
     [#;  13  #;  _11111111_11111111_11111111_111101      #x3ffffffd  30]
     [#;  14  #;  _11111111_11111111_11111110_1011         #xfffffeb  28]
     [#;  15  #;  _11111111_11111111_11111110_1100         #xfffffec  28]
     [#;  16  #;  _11111111_11111111_11111110_1101         #xfffffed  28]
     [#;  17  #;  _11111111_11111111_11111110_1110         #xfffffee  28]
     [#;  18  #;  _11111111_11111111_11111110_1111         #xfffffef  28]
     [#;  19  #;  _11111111_11111111_11111111_0000         #xffffff0  28]
     [#;  20  #;  _11111111_11111111_11111111_0001         #xffffff1  28]
     [#;  21  #;  _11111111_11111111_11111111_0010         #xffffff2  28]
     [#;  22  #;  _11111111_11111111_11111111_111110      #x3ffffffe  30]
     [#;  23  #;  _11111111_11111111_11111111_0011         #xffffff3  28]
     [#;  24  #;  _11111111_11111111_11111111_0100         #xffffff4  28]
     [#;  25  #;  _11111111_11111111_11111111_0101         #xffffff5  28]
     [#;  26  #;  _11111111_11111111_11111111_0110         #xffffff6  28]
     [#;  27  #;  _11111111_11111111_11111111_0111         #xffffff7  28]
     [#;  28  #;  _11111111_11111111_11111111_1000         #xffffff8  28]
     [#;  29  #;  _11111111_11111111_11111111_1001         #xffffff9  28]
     [#;  30  #;  _11111111_11111111_11111111_1010         #xffffffa  28]
     [#;  31  #;  _11111111_11111111_11111111_1011         #xffffffb  28]
     [#;  32  #;  _010100                                       #x14   6]
     [#;  33  #;  _11111110_00                                 #x3f8  10]
     [#;  34  #;  _11111110_01                                 #x3f9  10]
     [#;  35  #;  _11111111_1010                               #xffa  12]
     [#;  36  #;  _11111111_11001                             #x1ff9  13]
     [#;  37  #;  _010101                                       #x15   6]
     [#;  38  #;  _11111000                                     #xf8   8]
     [#;  39  #;  _11111111_010                                #x7fa  11]
     [#;  40  #;  _11111110_10                                 #x3fa  10]
     [#;  41  #;  _11111110_11                                 #x3fb  10]
     [#;  42  #;  _11111001                                     #xf9   8]
     [#;  43  #;  _11111111_011                                #x7fb  11]
     [#;  44  #;  _11111010                                     #xfa   8]
     [#;  45  #;  _010110                                       #x16   6]
     [#;  46  #;  _010111                                       #x17   6]
     [#;  47  #;  _011000                                       #x18   6]
     [#;  48  #;  _00000                                         #x0   5]
     [#;  49  #;  _00001                                         #x1   5]
     [#;  50  #;  _00010                                         #x2   5]
     [#;  51  #;  _011001                                       #x19   6]
     [#;  52  #;  _011010                                       #x1a   6]
     [#;  53  #;  _011011                                       #x1b   6]
     [#;  54  #;  _011100                                       #x1c   6]
     [#;  55  #;  _011101                                       #x1d   6]
     [#;  56  #;  _011110                                       #x1e   6]
     [#;  57  #;  _011111                                       #x1f   6]
     [#;  58  #;  _1011100                                      #x5c   7]
     [#;  59  #;  _11111011                                     #xfb   8]
     [#;  60  #;  _11111111_1111100                           #x7ffc  15]
     [#;  61  #;  _100000                                       #x20   6]
     [#;  62  #;  _11111111_1011                               #xffb  12]
     [#;  63  #;  _11111111_00                                 #x3fc  10]
     [#;  64  #;  _11111111_11010                             #x1ffa  13]
     [#;  65  #;  _100001                                       #x21   6]
     [#;  66  #;  _1011101                                      #x5d   7]
     [#;  67  #;  _1011110                                      #x5e   7]
     [#;  68  #;  _1011111                                      #x5f   7]
     [#;  69  #;  _1100000                                      #x60   7]
     [#;  70  #;  _1100001                                      #x61   7]
     [#;  71  #;  _1100010                                      #x62   7]
     [#;  72  #;  _1100011                                      #x63   7]
     [#;  73  #;  _1100100                                      #x64   7]
     [#;  74  #;  _1100101                                      #x65   7]
     [#;  75  #;  _1100110                                      #x66   7]
     [#;  76  #;  _1100111                                      #x67   7]
     [#;  77  #;  _1101000                                      #x68   7]
     [#;  78  #;  _1101001                                      #x69   7]
     [#;  79  #;  _1101010                                      #x6a   7]
     [#;  80  #;  _1101011                                      #x6b   7]
     [#;  81  #;  _1101100                                      #x6c   7]
     [#;  82  #;  _1101101                                      #x6d   7]
     [#;  83  #;  _1101110                                      #x6e   7]
     [#;  84  #;  _1101111                                      #x6f   7]
     [#;  85  #;  _1110000                                      #x70   7]
     [#;  86  #;  _1110001                                      #x71   7]
     [#;  87  #;  _1110010                                      #x72   7]
     [#;  88  #;  _11111100                                     #xfc   8]
     [#;  89  #;  _1110011                                      #x73   7]
     [#;  90  #;  _11111101                                     #xfd   8]
     [#;  91  #;  _11111111_11011                             #x1ffb  13]
     [#;  92  #;  _11111111_11111110_000                     #x7fff0  19]
     [#;  93  #;  _11111111_11100                             #x1ffc  13]
     [#;  94  #;  _11111111_111100                            #x3ffc  14]
     [#;  95  #;  _100010                                       #x22   6]
     [#;  96  #;  _11111111_1111101                           #x7ffd  15]
     [#;  97  #;  _00011                                         #x3   5]
     [#;  98  #;  _100011                                       #x23   6]
     [#;  99  #;  _00100                                         #x4   5]
     [#; 100  #;  _100100                                       #x24   6]
     [#; 101  #;  _00101                                         #x5   5]
     [#; 102  #;  _100101                                       #x25   6]
     [#; 103  #;  _100110                                       #x26   6]
     [#; 104  #;  _100111                                       #x27   6]
     [#; 105  #;  _00110                                         #x6   5]
     [#; 106  #;  _1110100                                      #x74   7]
     [#; 107  #;  _1110101                                      #x75   7]
     [#; 108  #;  _101000                                       #x28   6]
     [#; 109  #;  _101001                                       #x29   6]
     [#; 110  #;  _101010                                       #x2a   6]
     [#; 111  #;  _00111                                         #x7   5]
     [#; 112  #;  _101011                                       #x2b   6]
     [#; 113  #;  _1110110                                      #x76   7]
     [#; 114  #;  _101100                                       #x2c   6]
     [#; 115  #;  _01000                                         #x8   5]
     [#; 116  #;  _01001                                         #x9   5]
     [#; 117  #;  _101101                                       #x2d   6]
     [#; 118  #;  _1110111                                      #x77   7]
     [#; 119  #;  _1111000                                      #x78   7]
     [#; 120  #;  _1111001                                      #x79   7]
     [#; 121  #;  _1111010                                      #x7a   7]
     [#; 122  #;  _1111011                                      #x7b   7]
     [#; 123  #;  _11111111_1111110                           #x7ffe  15]
     [#; 124  #;  _11111111_100                                #x7fc  11]
     [#; 125  #;  _11111111_111101                            #x3ffd  14]
     [#; 126  #;  _11111111_11101                             #x1ffd  13]
     [#; 127  #;  _11111111_11111111_11111111_1100         #xffffffc  28]
     [#; 128  #;  _11111111_11111110_0110                    #xfffe6  20]
     [#; 129  #;  _11111111_11111111_010010                 #x3fffd2  22]
     [#; 130  #;  _11111111_11111110_0111                    #xfffe7  20]
     [#; 131  #;  _11111111_11111110_1000                    #xfffe8  20]
     [#; 132  #;  _11111111_11111111_010011                 #x3fffd3  22]
     [#; 133  #;  _11111111_11111111_010100                 #x3fffd4  22]
     [#; 134  #;  _11111111_11111111_010101                 #x3fffd5  22]
     [#; 135  #;  _11111111_11111111_1011001                #x7fffd9  23]
     [#; 136  #;  _11111111_11111111_010110                 #x3fffd6  22]
     [#; 137  #;  _11111111_11111111_1011010                #x7fffda  23]
     [#; 138  #;  _11111111_11111111_1011011                #x7fffdb  23]
     [#; 139  #;  _11111111_11111111_1011100                #x7fffdc  23]
     [#; 140  #;  _11111111_11111111_1011101                #x7fffdd  23]
     [#; 141  #;  _11111111_11111111_1011110                #x7fffde  23]
     [#; 142  #;  _11111111_11111111_11101011               #xffffeb  24]
     [#; 143  #;  _11111111_11111111_1011111                #x7fffdf  23]
     [#; 144  #;  _11111111_11111111_11101100               #xffffec  24]
     [#; 145  #;  _11111111_11111111_11101101               #xffffed  24]
     [#; 146  #;  _11111111_11111111_010111                 #x3fffd7  22]
     [#; 147  #;  _11111111_11111111_1100000                #x7fffe0  23]
     [#; 148  #;  _11111111_11111111_11101110               #xffffee  24]
     [#; 149  #;  _11111111_11111111_1100001                #x7fffe1  23]
     [#; 150  #;  _11111111_11111111_1100010                #x7fffe2  23]
     [#; 151  #;  _11111111_11111111_1100011                #x7fffe3  23]
     [#; 152  #;  _11111111_11111111_1100100                #x7fffe4  23]
     [#; 153  #;  _11111111_11111110_11100                  #x1fffdc  21]
     [#; 154  #;  _11111111_11111111_011000                 #x3fffd8  22]
     [#; 155  #;  _11111111_11111111_1100101                #x7fffe5  23]
     [#; 156  #;  _11111111_11111111_011001                 #x3fffd9  22]
     [#; 157  #;  _11111111_11111111_1100110                #x7fffe6  23]
     [#; 158  #;  _11111111_11111111_1100111                #x7fffe7  23]
     [#; 159  #;  _11111111_11111111_11101111               #xffffef  24]
     [#; 160  #;  _11111111_11111111_011010                 #x3fffda  22]
     [#; 161  #;  _11111111_11111110_11101                  #x1fffdd  21]
     [#; 162  #;  _11111111_11111110_1001                    #xfffe9  20]
     [#; 163  #;  _11111111_11111111_011011                 #x3fffdb  22]
     [#; 164  #;  _11111111_11111111_011100                 #x3fffdc  22]
     [#; 165  #;  _11111111_11111111_1101000                #x7fffe8  23]
     [#; 166  #;  _11111111_11111111_1101001                #x7fffe9  23]
     [#; 167  #;  _11111111_11111110_11110                  #x1fffde  21]
     [#; 168  #;  _11111111_11111111_1101010                #x7fffea  23]
     [#; 169  #;  _11111111_11111111_011101                 #x3fffdd  22]
     [#; 170  #;  _11111111_11111111_011110                 #x3fffde  22]
     [#; 171  #;  _11111111_11111111_11110000               #xfffff0  24]
     [#; 172  #;  _11111111_11111110_11111                  #x1fffdf  21]
     [#; 173  #;  _11111111_11111111_011111                 #x3fffdf  22]
     [#; 174  #;  _11111111_11111111_1101011                #x7fffeb  23]
     [#; 175  #;  _11111111_11111111_1101100                #x7fffec  23]
     [#; 176  #;  _11111111_11111111_00000                  #x1fffe0  21]
     [#; 177  #;  _11111111_11111111_00001                  #x1fffe1  21]
     [#; 178  #;  _11111111_11111111_100000                 #x3fffe0  22]
     [#; 179  #;  _11111111_11111111_00010                  #x1fffe2  21]
     [#; 180  #;  _11111111_11111111_1101101                #x7fffed  23]
     [#; 181  #;  _11111111_11111111_100001                 #x3fffe1  22]
     [#; 182  #;  _11111111_11111111_1101110                #x7fffee  23]
     [#; 183  #;  _11111111_11111111_1101111                #x7fffef  23]
     [#; 184  #;  _11111111_11111110_1010                    #xfffea  20]
     [#; 185  #;  _11111111_11111111_100010                 #x3fffe2  22]
     [#; 186  #;  _11111111_11111111_100011                 #x3fffe3  22]
     [#; 187  #;  _11111111_11111111_100100                 #x3fffe4  22]
     [#; 188  #;  _11111111_11111111_1110000                #x7ffff0  23]
     [#; 189  #;  _11111111_11111111_100101                 #x3fffe5  22]
     [#; 190  #;  _11111111_11111111_100110                 #x3fffe6  22]
     [#; 191  #;  _11111111_11111111_1110001                #x7ffff1  23]
     [#; 192  #;  _11111111_11111111_11111000_00           #x3ffffe0  26]
     [#; 193  #;  _11111111_11111111_11111000_01           #x3ffffe1  26]
     [#; 194  #;  _11111111_11111110_1011                    #xfffeb  20]
     [#; 195  #;  _11111111_11111110_001                     #x7fff1  19]
     [#; 196  #;  _11111111_11111111_100111                 #x3fffe7  22]
     [#; 197  #;  _11111111_11111111_1110010                #x7ffff2  23]
     [#; 198  #;  _11111111_11111111_101000                 #x3fffe8  22]
     [#; 199  #;  _11111111_11111111_11110110_0            #x1ffffec  25]
     [#; 200  #;  _11111111_11111111_11111000_10           #x3ffffe2  26]
     [#; 201  #;  _11111111_11111111_11111000_11           #x3ffffe3  26]
     [#; 202  #;  _11111111_11111111_11111001_00           #x3ffffe4  26]
     [#; 203  #;  _11111111_11111111_11111011_110          #x7ffffde  27]
     [#; 204  #;  _11111111_11111111_11111011_111          #x7ffffdf  27]
     [#; 205  #;  _11111111_11111111_11111001_01           #x3ffffe5  26]
     [#; 206  #;  _11111111_11111111_11110001               #xfffff1  24]
     [#; 207  #;  _11111111_11111111_11110110_1            #x1ffffed  25]
     [#; 208  #;  _11111111_11111110_010                     #x7fff2  19]
     [#; 209  #;  _11111111_11111111_00011                  #x1fffe3  21]
     [#; 210  #;  _11111111_11111111_11111001_10           #x3ffffe6  26]
     [#; 211  #;  _11111111_11111111_11111100_000          #x7ffffe0  27]
     [#; 212  #;  _11111111_11111111_11111100_001          #x7ffffe1  27]
     [#; 213  #;  _11111111_11111111_11111001_11           #x3ffffe7  26]
     [#; 214  #;  _11111111_11111111_11111100_010          #x7ffffe2  27]
     [#; 215  #;  _11111111_11111111_11110010               #xfffff2  24]
     [#; 216  #;  _11111111_11111111_00100                  #x1fffe4  21]
     [#; 217  #;  _11111111_11111111_00101                  #x1fffe5  21]
     [#; 218  #;  _11111111_11111111_11111010_00           #x3ffffe8  26]
     [#; 219  #;  _11111111_11111111_11111010_01           #x3ffffe9  26]
     [#; 220  #;  _11111111_11111111_11111111_1101         #xffffffd  28]
     [#; 221  #;  _11111111_11111111_11111100_011          #x7ffffe3  27]
     [#; 222  #;  _11111111_11111111_11111100_100          #x7ffffe4  27]
     [#; 223  #;  _11111111_11111111_11111100_101          #x7ffffe5  27]
     [#; 224  #;  _11111111_11111110_1100                    #xfffec  20]
     [#; 225  #;  _11111111_11111111_11110011               #xfffff3  24]
     [#; 226  #;  _11111111_11111110_1101                    #xfffed  20]
     [#; 227  #;  _11111111_11111111_00110                  #x1fffe6  21]
     [#; 228  #;  _11111111_11111111_101001                 #x3fffe9  22]
     [#; 229  #;  _11111111_11111111_00111                  #x1fffe7  21]
     [#; 230  #;  _11111111_11111111_01000                  #x1fffe8  21]
     [#; 231  #;  _11111111_11111111_1110011                #x7ffff3  23]
     [#; 232  #;  _11111111_11111111_101010                 #x3fffea  22]
     [#; 233  #;  _11111111_11111111_101011                 #x3fffeb  22]
     [#; 234  #;  _11111111_11111111_11110111_0            #x1ffffee  25]
     [#; 235  #;  _11111111_11111111_11110111_1            #x1ffffef  25]
     [#; 236  #;  _11111111_11111111_11110100               #xfffff4  24]
     [#; 237  #;  _11111111_11111111_11110101               #xfffff5  24]
     [#; 238  #;  _11111111_11111111_11111010_10           #x3ffffea  26]
     [#; 239  #;  _11111111_11111111_1110100                #x7ffff4  23]
     [#; 240  #;  _11111111_11111111_11111010_11           #x3ffffeb  26]
     [#; 241  #;  _11111111_11111111_11111100_110          #x7ffffe6  27]
     [#; 242  #;  _11111111_11111111_11111011_00           #x3ffffec  26]
     [#; 243  #;  _11111111_11111111_11111011_01           #x3ffffed  26]
     [#; 244  #;  _11111111_11111111_11111100_111          #x7ffffe7  27]
     [#; 245  #;  _11111111_11111111_11111101_000          #x7ffffe8  27]
     [#; 246  #;  _11111111_11111111_11111101_001          #x7ffffe9  27]
     [#; 247  #;  _11111111_11111111_11111101_010          #x7ffffea  27]
     [#; 248  #;  _11111111_11111111_11111101_011          #x7ffffeb  27]
     [#; 249  #;  _11111111_11111111_11111111_1110         #xffffffe  28]
     [#; 250  #;  _11111111_11111111_11111101_100          #x7ffffec  27]
     [#; 251  #;  _11111111_11111111_11111101_101          #x7ffffed  27]
     [#; 252  #;  _11111111_11111111_11111101_110          #x7ffffee  27]
     [#; 253  #;  _11111111_11111111_11111101_111          #x7ffffef  27]
     [#; 254  #;  _11111111_11111111_11111110_000          #x7fffff0  27]
     [#; 255  #;  _11111111_11111111_11111011_10           #x3ffffee  26]
     [#; 256  #;  _11111111_11111111_11111111_111111      #x3fffffff  30]])

(define EOS-CODE-SBV (make-be-sbv #x3fffffff 30))

(define h-table
  (for/vector ([e (in-vector h-table-src)])
    (make-be-sbv (car e) (cadr e))))

;; A HuffmanTree is one of
;; - Nat
;; - (cons HuffmanTree HuffmanTree) -- (cons 0-branch 1-branch)

(define h-decode-tree
  (let ()
    (define init-codes
      (for/list ([code (in-vector h-table)] [value (in-naturals)])
        (cons code value)))
    (define (take-entries codes bit)
      (for/list ([e (in-list codes)] #:when (= (sbv-car (car e)) bit))
        (cons (sbv-cdr (car e)) (cdr e))))
    (let loop ([codes init-codes])
      (cond [(= (length codes) 1)
             (cdr (car codes))]
            [else
             (cons (loop (take-entries codes 0))
                   (loop (take-entries codes 1)))]))))

(define (h-encode bs)
  (let-values ([(enc _s _e) (h-encode* bs)]) enc))
(define (h-encode* bs)
  (define bbuf (make-bitbuffer))
  (for ([b (in-bytes bs)])
    (bitbuffer-write-sbv bbuf (h-encode-byte b)))
  (bitbuffer-get-bytes bbuf #:pad #xFF))
(define (h-encode-byte b)
  (vector-ref h-table b))

(define (h-decode bs [start-biti 0] [end-biti (* 8 (bytes-length bs))])
  (let-values ([(dec biti) (h-decode* bs start-biti end-biti)]) dec))
(define (h-decode* bs [start-biti 0] [end-biti (* 8 (bytes-length bs))])
  (define out (open-output-bytes))
  (define (decode-byte biti)
    (let loop ([biti biti] [tree h-decode-tree] [rprefix empty-sbv])
      (cond [(pair? tree)
             (cond [(< biti end-biti)
                    (if (bytes-be-bit-set? bs biti)
                        (loop (add1 biti) (cdr tree) (sbv-cons 1 rprefix))
                        (loop (add1 biti) (car tree) (sbv-cons 0 rprefix)))]
                   [(and (< (sbv-length rprefix) 8)
                         (sbv-prefix? (sbv-reverse rprefix) EOS-CODE-SBV))
                    (done biti 'partial)]
                   [else (error 'h-decode "malformed Huffman encoding")])]
            [(byte? tree)
             (write-byte tree out)
             (decode-rest biti)]
            [else (error 'h-decode "malformed Huffman encoding (explicit EOS)")])))
  (define (decode-rest biti)
    (cond [(< biti end-biti)
           (decode-byte biti)]
          [else (done biti 'complete)]))
  (define (done biti eos?)
    (values (get-output-bytes out) biti))
  (decode-rest start-biti))

;; ============================================================

(module+ test
  (require racket/port
           racket/pretty)
  (pretty-print-columns 80)

  (define ex-headers
    '((#":method" #"GET")
      (#":authority" #"www.racket-lang.org")
      (#":path" #"/")
      (#":scheme" #"https")
      (#"host" #"www.racket-lang.org")
      (#"user-agent" #"Racket (http123)")
      (#"x-racket-thing" #"syntax-local-something")
      (#"x-expect-not-huffman" #"{|~~ $& ~~|}")
      (#"x-expect-huffman" #"00001111 00001111 00001111")))
  (define enc-header-reps (encode-headers* ex-headers (make-dtable 512)))
  ;(pretty-print enc-header-reps)

  (define hblock
    (call-with-output-bytes
     (lambda (out) (encode-headers out ex-headers (make-dtable 512)))))
  ;; hblock

  (define dec-header-reps
    (read-header-reps (make-binary-reader (open-input-bytes hblock)
                                          #:limit (bytes-length hblock))))
  (equal? dec-header-reps enc-header-reps)
  ;(pretty-print dec-header-reps)

  (define dec-headers
    (decode-headers (make-binary-reader (open-input-bytes hblock)
                                        #:limit (bytes-length hblock))
                    (make-dtable 512)))
  ;(pretty-print dec-headers)
  (equal? dec-headers ex-headers))
