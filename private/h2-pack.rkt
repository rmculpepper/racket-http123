#lang racket/base
(require racket/match
         racket/vector
         binaryio/reader
         binaryio/bitvector
         binaryio/prefixcode)
(provide (all-defined-out))

;; Reference:
;; - https://tools.ietf.org/html/rfc7541

;; Note on normalization: RFC 7541 says header names are treated as
;; opaque sequences of octets. But HTTP/2 .... (FIXME)

;; ----------------------------------------

;; HeaderEntry = (list Bytes Bytes) | (list Bytes Bytes 'never-add)

;; encode-header : (Listof HeaderEntry) State -> Void
(define (encode-header hfields dt #:who [who 'encode-header]
                       #:huffman? [huffman? #t]
                       #:overrides [overrides #hash()])
  (define out (open-output-bytes))
  (define hfield-reps (encode-header* hfields dt overrides #:who who))
  (for ([hrep (in-list hfield-reps)])
    (write-header-rep out hrep huffman?))
  (get-output-bytes out))

(define (encode-header* hfields dt [overrides #hash()] #:who [who 'encode-header*])
  (for/list ([hfield (in-list hfields)])
    (encode-hfield who hfield dt overrides)))

(define (encode-hfield who hfield dt overrides)
  (match hfield
    [(list key value 'never-add)
     (header:literal 'never-add key value)]
    [(list key value)
     (define key-index (or (hash-ref key-table key #f)
                           (dtable-find-key dt key (dtable-adjustment))))
     (cond [(hash-ref key+value-table (list key value) #f)
            => (lambda (index) (header:indexed index))]
           [(dtable-find dt (list key value) (dtable-adjustment))
            => (lambda (index) (header:indexed index))]
           [else
            (case (key-index-mode overrides key)
              [(yes)
               (dtable-add! dt (list key value))
               (header:literal 'add (or key-index key) value)]
              [(never)
               (header:literal 'never-add (or key-index key) value)]
              [else
               (header:literal 'no-add (or key-index key) value)])])]))

;; decode-header : Bytes DTable -> (Listof NormalizedHeader)
(define (decode-header bs dt)
  (define hfield-reps (read-hfield-reps bs))
  (for/list ([hrep (in-list hfield-reps)])
    (decode-hfield hrep dt)))

(define (read-hfield-reps bs)
  (define br (make-binary-reader (open-input-bytes bs) #:limit (bytes-length bs)))
  (let loop ()
    (cond [(b-at-limit? br) null]
          [else (cons (read-hfield-rep br) (loop))])))

(define (decode-hfield h dt)
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

;; References:
;; - https://github.com/nghttp2/nghttp2/blob/master/lib/nghttp2_hd.c
;;   Their policy:
;;     NEVER for Authorization (all), Cookie (w/ length < 20) because
;;       they might contain low-entropy secrets
;;     NO to :path, Age, Content-Length, ETag, If-Modified-Since, If-None-Match,
;;       Location, Set-Cookie

;; Default policy:
;; Default to indexing Authorization and Cookie. Maybe override if
;; client expects to use low-entropy secrets.

(define default-indexing-policy
  #hash((default                . no)
        (#":authority"          . yes)
        (#":path"               . no)  ;; maybe yes for APIs
        (#"accept-charset"      . yes)
        (#"accept-language"     . yes)
        (#"age"                 . no)
        (#"authorization"       . yes)
        (#"cache-control"       . yes)
        (#"content-length"      . no)
        (#"cookie"              . yes)
        (#"content-type"        . yes)
        (#"etag"                . no)
        (#"host"                . yes) ;; should not appear!
        (#"if-modified-since"   . no)
        (#"if-none-match"       . no)
        (#"location"            . no)
        (#"proxy-authorization" . yes)
        (#"referer"             . yes)
        (#"set-cookie"          . no)
        (#"user-agent"          . yes)))

(define (key-index-mode overrides key)
  (or (hash-ref overrides key #f)
      (hash-ref default-indexing-policy key #f)
      (hash-ref overrides 'default #f)
      (hash-ref default-indexing-policy key 'no)))

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
         (set-dtable-count! dt (add1 count))
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

(define (write-string-literal out bs huffman?)
  (define enc (and huffman?
                   (<= H-ENC-MIN-LEN (bytes-length bs) H-ENC-MAX-LEN)
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

(define (read-hfield-rep br)
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

(define (write-header-rep out h huffman?)
  (match h
    [(header:indexed index)
     (write-intrep out 7 #b1 index)]
    [(header:literal mode key value)
     (define keyindex (if (bytes? key) 0 key))
     (case mode
       [(add) (write-intrep out 6 #b01 keyindex)]
       [(no-add) (write-intrep out 4 #b0000 keyindex)]
       [(never-add) (write-intrep out 4 #b0001 keyindex)])
     (when (bytes? key) (write-string-literal out key huffman?))
     (write-string-literal out value huffman?)]
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
  '[;;                   code as bits                   as hex     len
    ;;  sym             aligned to MSB                  aligned     in
    ;;                                                  to LSB     bits
    [#;   0  #;  11111111_11000                             #x1ff8  13]
    [#;   1  #;  11111111_11111111_1011000                #x7fffd8  23]
    [#;   2  #;  11111111_11111111_11111110_0010         #xfffffe2  28]
    [#;   3  #;  11111111_11111111_11111110_0011         #xfffffe3  28]
    [#;   4  #;  11111111_11111111_11111110_0100         #xfffffe4  28]
    [#;   5  #;  11111111_11111111_11111110_0101         #xfffffe5  28]
    [#;   6  #;  11111111_11111111_11111110_0110         #xfffffe6  28]
    [#;   7  #;  11111111_11111111_11111110_0111         #xfffffe7  28]
    [#;   8  #;  11111111_11111111_11111110_1000         #xfffffe8  28]
    [#;   9  #;  11111111_11111111_11101010               #xffffea  24]
    [#;  10  #;  11111111_11111111_11111111_111100      #x3ffffffc  30]
    [#;  11  #;  11111111_11111111_11111110_1001         #xfffffe9  28]
    [#;  12  #;  11111111_11111111_11111110_1010         #xfffffea  28]
    [#;  13  #;  11111111_11111111_11111111_111101      #x3ffffffd  30]
    [#;  14  #;  11111111_11111111_11111110_1011         #xfffffeb  28]
    [#;  15  #;  11111111_11111111_11111110_1100         #xfffffec  28]
    [#;  16  #;  11111111_11111111_11111110_1101         #xfffffed  28]
    [#;  17  #;  11111111_11111111_11111110_1110         #xfffffee  28]
    [#;  18  #;  11111111_11111111_11111110_1111         #xfffffef  28]
    [#;  19  #;  11111111_11111111_11111111_0000         #xffffff0  28]
    [#;  20  #;  11111111_11111111_11111111_0001         #xffffff1  28]
    [#;  21  #;  11111111_11111111_11111111_0010         #xffffff2  28]
    [#;  22  #;  11111111_11111111_11111111_111110      #x3ffffffe  30]
    [#;  23  #;  11111111_11111111_11111111_0011         #xffffff3  28]
    [#;  24  #;  11111111_11111111_11111111_0100         #xffffff4  28]
    [#;  25  #;  11111111_11111111_11111111_0101         #xffffff5  28]
    [#;  26  #;  11111111_11111111_11111111_0110         #xffffff6  28]
    [#;  27  #;  11111111_11111111_11111111_0111         #xffffff7  28]
    [#;  28  #;  11111111_11111111_11111111_1000         #xffffff8  28]
    [#;  29  #;  11111111_11111111_11111111_1001         #xffffff9  28]
    [#;  30  #;  11111111_11111111_11111111_1010         #xffffffa  28]
    [#;  31  #;  11111111_11111111_11111111_1011         #xffffffb  28]
    [#;  32  #;  010100                                       #x14   6]
    [#;  33  #;  11111110_00                                 #x3f8  10]
    [#;  34  #;  11111110_01                                 #x3f9  10]
    [#;  35  #;  11111111_1010                               #xffa  12]
    [#;  36  #;  11111111_11001                             #x1ff9  13]
    [#;  37  #;  010101                                       #x15   6]
    [#;  38  #;  11111000                                     #xf8   8]
    [#;  39  #;  11111111_010                                #x7fa  11]
    [#;  40  #;  11111110_10                                 #x3fa  10]
    [#;  41  #;  11111110_11                                 #x3fb  10]
    [#;  42  #;  11111001                                     #xf9   8]
    [#;  43  #;  11111111_011                                #x7fb  11]
    [#;  44  #;  11111010                                     #xfa   8]
    [#;  45  #;  010110                                       #x16   6]
    [#;  46  #;  010111                                       #x17   6]
    [#;  47  #;  011000                                       #x18   6]
    [#;  48  #;  00000                                         #x0   5]
    [#;  49  #;  00001                                         #x1   5]
    [#;  50  #;  00010                                         #x2   5]
    [#;  51  #;  011001                                       #x19   6]
    [#;  52  #;  011010                                       #x1a   6]
    [#;  53  #;  011011                                       #x1b   6]
    [#;  54  #;  011100                                       #x1c   6]
    [#;  55  #;  011101                                       #x1d   6]
    [#;  56  #;  011110                                       #x1e   6]
    [#;  57  #;  011111                                       #x1f   6]
    [#;  58  #;  1011100                                      #x5c   7]
    [#;  59  #;  11111011                                     #xfb   8]
    [#;  60  #;  11111111_1111100                           #x7ffc  15]
    [#;  61  #;  100000                                       #x20   6]
    [#;  62  #;  11111111_1011                               #xffb  12]
    [#;  63  #;  11111111_00                                 #x3fc  10]
    [#;  64  #;  11111111_11010                             #x1ffa  13]
    [#;  65  #;  100001                                       #x21   6]
    [#;  66  #;  1011101                                      #x5d   7]
    [#;  67  #;  1011110                                      #x5e   7]
    [#;  68  #;  1011111                                      #x5f   7]
    [#;  69  #;  1100000                                      #x60   7]
    [#;  70  #;  1100001                                      #x61   7]
    [#;  71  #;  1100010                                      #x62   7]
    [#;  72  #;  1100011                                      #x63   7]
    [#;  73  #;  1100100                                      #x64   7]
    [#;  74  #;  1100101                                      #x65   7]
    [#;  75  #;  1100110                                      #x66   7]
    [#;  76  #;  1100111                                      #x67   7]
    [#;  77  #;  1101000                                      #x68   7]
    [#;  78  #;  1101001                                      #x69   7]
    [#;  79  #;  1101010                                      #x6a   7]
    [#;  80  #;  1101011                                      #x6b   7]
    [#;  81  #;  1101100                                      #x6c   7]
    [#;  82  #;  1101101                                      #x6d   7]
    [#;  83  #;  1101110                                      #x6e   7]
    [#;  84  #;  1101111                                      #x6f   7]
    [#;  85  #;  1110000                                      #x70   7]
    [#;  86  #;  1110001                                      #x71   7]
    [#;  87  #;  1110010                                      #x72   7]
    [#;  88  #;  11111100                                     #xfc   8]
    [#;  89  #;  1110011                                      #x73   7]
    [#;  90  #;  11111101                                     #xfd   8]
    [#;  91  #;  11111111_11011                             #x1ffb  13]
    [#;  92  #;  11111111_11111110_000                     #x7fff0  19]
    [#;  93  #;  11111111_11100                             #x1ffc  13]
    [#;  94  #;  11111111_111100                            #x3ffc  14]
    [#;  95  #;  100010                                       #x22   6]
    [#;  96  #;  11111111_1111101                           #x7ffd  15]
    [#;  97  #;  00011                                         #x3   5]
    [#;  98  #;  100011                                       #x23   6]
    [#;  99  #;  00100                                         #x4   5]
    [#; 100  #;  100100                                       #x24   6]
    [#; 101  #;  00101                                         #x5   5]
    [#; 102  #;  100101                                       #x25   6]
    [#; 103  #;  100110                                       #x26   6]
    [#; 104  #;  100111                                       #x27   6]
    [#; 105  #;  00110                                         #x6   5]
    [#; 106  #;  1110100                                      #x74   7]
    [#; 107  #;  1110101                                      #x75   7]
    [#; 108  #;  101000                                       #x28   6]
    [#; 109  #;  101001                                       #x29   6]
    [#; 110  #;  101010                                       #x2a   6]
    [#; 111  #;  00111                                         #x7   5]
    [#; 112  #;  101011                                       #x2b   6]
    [#; 113  #;  1110110                                      #x76   7]
    [#; 114  #;  101100                                       #x2c   6]
    [#; 115  #;  01000                                         #x8   5]
    [#; 116  #;  01001                                         #x9   5]
    [#; 117  #;  101101                                       #x2d   6]
    [#; 118  #;  1110111                                      #x77   7]
    [#; 119  #;  1111000                                      #x78   7]
    [#; 120  #;  1111001                                      #x79   7]
    [#; 121  #;  1111010                                      #x7a   7]
    [#; 122  #;  1111011                                      #x7b   7]
    [#; 123  #;  11111111_1111110                           #x7ffe  15]
    [#; 124  #;  11111111_100                                #x7fc  11]
    [#; 125  #;  11111111_111101                            #x3ffd  14]
    [#; 126  #;  11111111_11101                             #x1ffd  13]
    [#; 127  #;  11111111_11111111_11111111_1100         #xffffffc  28]
    [#; 128  #;  11111111_11111110_0110                    #xfffe6  20]
    [#; 129  #;  11111111_11111111_010010                 #x3fffd2  22]
    [#; 130  #;  11111111_11111110_0111                    #xfffe7  20]
    [#; 131  #;  11111111_11111110_1000                    #xfffe8  20]
    [#; 132  #;  11111111_11111111_010011                 #x3fffd3  22]
    [#; 133  #;  11111111_11111111_010100                 #x3fffd4  22]
    [#; 134  #;  11111111_11111111_010101                 #x3fffd5  22]
    [#; 135  #;  11111111_11111111_1011001                #x7fffd9  23]
    [#; 136  #;  11111111_11111111_010110                 #x3fffd6  22]
    [#; 137  #;  11111111_11111111_1011010                #x7fffda  23]
    [#; 138  #;  11111111_11111111_1011011                #x7fffdb  23]
    [#; 139  #;  11111111_11111111_1011100                #x7fffdc  23]
    [#; 140  #;  11111111_11111111_1011101                #x7fffdd  23]
    [#; 141  #;  11111111_11111111_1011110                #x7fffde  23]
    [#; 142  #;  11111111_11111111_11101011               #xffffeb  24]
    [#; 143  #;  11111111_11111111_1011111                #x7fffdf  23]
    [#; 144  #;  11111111_11111111_11101100               #xffffec  24]
    [#; 145  #;  11111111_11111111_11101101               #xffffed  24]
    [#; 146  #;  11111111_11111111_010111                 #x3fffd7  22]
    [#; 147  #;  11111111_11111111_1100000                #x7fffe0  23]
    [#; 148  #;  11111111_11111111_11101110               #xffffee  24]
    [#; 149  #;  11111111_11111111_1100001                #x7fffe1  23]
    [#; 150  #;  11111111_11111111_1100010                #x7fffe2  23]
    [#; 151  #;  11111111_11111111_1100011                #x7fffe3  23]
    [#; 152  #;  11111111_11111111_1100100                #x7fffe4  23]
    [#; 153  #;  11111111_11111110_11100                  #x1fffdc  21]
    [#; 154  #;  11111111_11111111_011000                 #x3fffd8  22]
    [#; 155  #;  11111111_11111111_1100101                #x7fffe5  23]
    [#; 156  #;  11111111_11111111_011001                 #x3fffd9  22]
    [#; 157  #;  11111111_11111111_1100110                #x7fffe6  23]
    [#; 158  #;  11111111_11111111_1100111                #x7fffe7  23]
    [#; 159  #;  11111111_11111111_11101111               #xffffef  24]
    [#; 160  #;  11111111_11111111_011010                 #x3fffda  22]
    [#; 161  #;  11111111_11111110_11101                  #x1fffdd  21]
    [#; 162  #;  11111111_11111110_1001                    #xfffe9  20]
    [#; 163  #;  11111111_11111111_011011                 #x3fffdb  22]
    [#; 164  #;  11111111_11111111_011100                 #x3fffdc  22]
    [#; 165  #;  11111111_11111111_1101000                #x7fffe8  23]
    [#; 166  #;  11111111_11111111_1101001                #x7fffe9  23]
    [#; 167  #;  11111111_11111110_11110                  #x1fffde  21]
    [#; 168  #;  11111111_11111111_1101010                #x7fffea  23]
    [#; 169  #;  11111111_11111111_011101                 #x3fffdd  22]
    [#; 170  #;  11111111_11111111_011110                 #x3fffde  22]
    [#; 171  #;  11111111_11111111_11110000               #xfffff0  24]
    [#; 172  #;  11111111_11111110_11111                  #x1fffdf  21]
    [#; 173  #;  11111111_11111111_011111                 #x3fffdf  22]
    [#; 174  #;  11111111_11111111_1101011                #x7fffeb  23]
    [#; 175  #;  11111111_11111111_1101100                #x7fffec  23]
    [#; 176  #;  11111111_11111111_00000                  #x1fffe0  21]
    [#; 177  #;  11111111_11111111_00001                  #x1fffe1  21]
    [#; 178  #;  11111111_11111111_100000                 #x3fffe0  22]
    [#; 179  #;  11111111_11111111_00010                  #x1fffe2  21]
    [#; 180  #;  11111111_11111111_1101101                #x7fffed  23]
    [#; 181  #;  11111111_11111111_100001                 #x3fffe1  22]
    [#; 182  #;  11111111_11111111_1101110                #x7fffee  23]
    [#; 183  #;  11111111_11111111_1101111                #x7fffef  23]
    [#; 184  #;  11111111_11111110_1010                    #xfffea  20]
    [#; 185  #;  11111111_11111111_100010                 #x3fffe2  22]
    [#; 186  #;  11111111_11111111_100011                 #x3fffe3  22]
    [#; 187  #;  11111111_11111111_100100                 #x3fffe4  22]
    [#; 188  #;  11111111_11111111_1110000                #x7ffff0  23]
    [#; 189  #;  11111111_11111111_100101                 #x3fffe5  22]
    [#; 190  #;  11111111_11111111_100110                 #x3fffe6  22]
    [#; 191  #;  11111111_11111111_1110001                #x7ffff1  23]
    [#; 192  #;  11111111_11111111_11111000_00           #x3ffffe0  26]
    [#; 193  #;  11111111_11111111_11111000_01           #x3ffffe1  26]
    [#; 194  #;  11111111_11111110_1011                    #xfffeb  20]
    [#; 195  #;  11111111_11111110_001                     #x7fff1  19]
    [#; 196  #;  11111111_11111111_100111                 #x3fffe7  22]
    [#; 197  #;  11111111_11111111_1110010                #x7ffff2  23]
    [#; 198  #;  11111111_11111111_101000                 #x3fffe8  22]
    [#; 199  #;  11111111_11111111_11110110_0            #x1ffffec  25]
    [#; 200  #;  11111111_11111111_11111000_10           #x3ffffe2  26]
    [#; 201  #;  11111111_11111111_11111000_11           #x3ffffe3  26]
    [#; 202  #;  11111111_11111111_11111001_00           #x3ffffe4  26]
    [#; 203  #;  11111111_11111111_11111011_110          #x7ffffde  27]
    [#; 204  #;  11111111_11111111_11111011_111          #x7ffffdf  27]
    [#; 205  #;  11111111_11111111_11111001_01           #x3ffffe5  26]
    [#; 206  #;  11111111_11111111_11110001               #xfffff1  24]
    [#; 207  #;  11111111_11111111_11110110_1            #x1ffffed  25]
    [#; 208  #;  11111111_11111110_010                     #x7fff2  19]
    [#; 209  #;  11111111_11111111_00011                  #x1fffe3  21]
    [#; 210  #;  11111111_11111111_11111001_10           #x3ffffe6  26]
    [#; 211  #;  11111111_11111111_11111100_000          #x7ffffe0  27]
    [#; 212  #;  11111111_11111111_11111100_001          #x7ffffe1  27]
    [#; 213  #;  11111111_11111111_11111001_11           #x3ffffe7  26]
    [#; 214  #;  11111111_11111111_11111100_010          #x7ffffe2  27]
    [#; 215  #;  11111111_11111111_11110010               #xfffff2  24]
    [#; 216  #;  11111111_11111111_00100                  #x1fffe4  21]
    [#; 217  #;  11111111_11111111_00101                  #x1fffe5  21]
    [#; 218  #;  11111111_11111111_11111010_00           #x3ffffe8  26]
    [#; 219  #;  11111111_11111111_11111010_01           #x3ffffe9  26]
    [#; 220  #;  11111111_11111111_11111111_1101         #xffffffd  28]
    [#; 221  #;  11111111_11111111_11111100_011          #x7ffffe3  27]
    [#; 222  #;  11111111_11111111_11111100_100          #x7ffffe4  27]
    [#; 223  #;  11111111_11111111_11111100_101          #x7ffffe5  27]
    [#; 224  #;  11111111_11111110_1100                    #xfffec  20]
    [#; 225  #;  11111111_11111111_11110011               #xfffff3  24]
    [#; 226  #;  11111111_11111110_1101                    #xfffed  20]
    [#; 227  #;  11111111_11111111_00110                  #x1fffe6  21]
    [#; 228  #;  11111111_11111111_101001                 #x3fffe9  22]
    [#; 229  #;  11111111_11111111_00111                  #x1fffe7  21]
    [#; 230  #;  11111111_11111111_01000                  #x1fffe8  21]
    [#; 231  #;  11111111_11111111_1110011                #x7ffff3  23]
    [#; 232  #;  11111111_11111111_101010                 #x3fffea  22]
    [#; 233  #;  11111111_11111111_101011                 #x3fffeb  22]
    [#; 234  #;  11111111_11111111_11110111_0            #x1ffffee  25]
    [#; 235  #;  11111111_11111111_11110111_1            #x1ffffef  25]
    [#; 236  #;  11111111_11111111_11110100               #xfffff4  24]
    [#; 237  #;  11111111_11111111_11110101               #xfffff5  24]
    [#; 238  #;  11111111_11111111_11111010_10           #x3ffffea  26]
    [#; 239  #;  11111111_11111111_1110100                #x7ffff4  23]
    [#; 240  #;  11111111_11111111_11111010_11           #x3ffffeb  26]
    [#; 241  #;  11111111_11111111_11111100_110          #x7ffffe6  27]
    [#; 242  #;  11111111_11111111_11111011_00           #x3ffffec  26]
    [#; 243  #;  11111111_11111111_11111011_01           #x3ffffed  26]
    [#; 244  #;  11111111_11111111_11111100_111          #x7ffffe7  27]
    [#; 245  #;  11111111_11111111_11111101_000          #x7ffffe8  27]
    [#; 246  #;  11111111_11111111_11111101_001          #x7ffffe9  27]
    [#; 247  #;  11111111_11111111_11111101_010          #x7ffffea  27]
    [#; 248  #;  11111111_11111111_11111101_011          #x7ffffeb  27]
    [#; 249  #;  11111111_11111111_11111111_1110         #xffffffe  28]
    [#; 250  #;  11111111_11111111_11111101_100          #x7ffffec  27]
    [#; 251  #;  11111111_11111111_11111101_101          #x7ffffed  27]
    [#; 252  #;  11111111_11111111_11111101_110          #x7ffffee  27]
    [#; 253  #;  11111111_11111111_11111101_111          #x7ffffef  27]
    [#; 254  #;  11111111_11111111_11111110_000          #x7fffff0  27]
    [#; 255  #;  11111111_11111111_11111011_10           #x3ffffee  26]
    #;[ 256  #;  11111111_11111111_11111111_111111      #x3fffffff  30]])

(define EOS-CODE (make-be-sbv #x3fffffff 30))

(define h-table
  (for/vector ([e (in-list h-table-src)])
    (make-be-sbv (car e) (cadr e))))
(define h-decode-tree
  (prefixcode-build-decode-tree h-table))

(define (h-encode bs)
  (define-values (enc encbits)
    (prefixcode-encode h-table bs #:pad EOS-CODE))
  enc)

(define (h-decode bs)
  (prefixcode-decode h-decode-tree bs #:end EOS-CODE))

;; ============================================================

(module+ test
  (require racket/port
           racket/pretty)
  (pretty-print-columns 80)

  (define ex-header
    '((#":method" #"GET")
      (#":authority" #"www.racket-lang.org")
      (#":path" #"/")
      (#":scheme" #"https")
      (#"host" #"www.racket-lang.org")
      (#"user-agent" #"Racket (http123)")
      (#"x-racket-thing" #"syntax-local-something")
      (#"x-expect-not-huffman" #"{|~~ $& ~~|}")
      (#"x-expect-huffman" #"00001111 00001111 00001111")))
  (define enc-header-reps (encode-header* ex-header (make-dtable 512)))
  ;(pretty-print enc-header-reps)

  (define hblock (encode-header ex-header (make-dtable 512)))
  ;; hblock

  (define dec-header-reps (read-hfield-reps hblock))
  (equal? dec-header-reps enc-header-reps)
  ;(pretty-print dec-header-reps)

  (define dec-header (decode-header hblock (make-dtable 512)))
  ;(pretty-print dec-header)
  (equal? dec-header ex-header)

  ;; ============================================================

  (require file/sha1)
  ;;(define b1 (encode-header '((#"custom-key" #"custom-header")) (make-dtable 4096)))
  ;(define b1 (encode-header '((#":path" #"/sample/path")) (make-dtable 4096)))
  ;b1
  ;(bytes->hex-string b1)

  (define dt-shared (make-dtable 4096)) ;; Note: shared between consecutive examples!
  (define b1 (encode-header '((#":method" #"GET")
                              (#":scheme" #"http")
                              (#":path" #"/")
                              (#":authority" #"www.example.com"))
                            dt-shared #:huffman? #f))
  (equal? (bytes->hex-string b1)
          "828684410f7777772e6578616d706c652e636f6d")

  (define b2 (encode-header '((#":method" #"GET")
                              (#":scheme" #"http")
                              (#":path" #"/")
                              (#":authority" #"www.example.com")
                              (#"cache-control" #"no-cache"))
                            dt-shared #:huffman? #f))
  (equal? (bytes->hex-string b2)
          "828684be58086e6f2d6361636865")

  (define b3 (encode-header '((#":method" #"GET")
                              (#":scheme" #"https")
                              (#":path" #"/index.html")
                              (#":authority" #"www.example.com")
                              (#"custom-key" #"custom-value"))
                            dt-shared #:huffman? #f #:overrides #hash((#"custom-key" . yes))))
  (equal? (bytes->hex-string b3)
          "828785bf400a637573746f6d2d6b65790c637573746f6d2d76616c7565")

  (define dt2 (make-dtable 4096)) ;; Note: shared between consecutive examples!
  (define h1 (encode-header '((#":method" #"GET")
                              (#":scheme" #"http")
                              (#":path" #"/")
                              (#":authority" #"www.example.com"))
                            dt2 #:huffman? #t))
  (equal? (bytes->hex-string h1)
          "828684418cf1e3c2e5f23a6ba0ab90f4ff")

  (define h2 (encode-header '((#":method" #"GET")
                              (#":scheme" #"http")
                              (#":path" #"/")
                              (#":authority" #"www.example.com")
                              (#"cache-control" #"no-cache"))
                            dt2 #:huffman? #t))
  (equal? (bytes->hex-string h2)
          "828684be5886a8eb10649cbf")

  (define h3 (encode-header '((#":method" #"GET")
                              (#":scheme" #"https")
                              (#":path" #"/index.html")
                              (#":authority" #"www.example.com")
                              (#"custom-key" #"custom-value"))
                            dt2 #:huffman? #t #:overrides #hash((#"custom-key" . yes))))
  (equal? (bytes->hex-string h3)
          "828785bf408825a849e95ba97d7f8925a849e95bb8e8b4bf")

  ;(bytes->hex-string (h-encode #"no-cache"))
  ;(h-decode (hex-string->bytes "a8eb10649cbf"))

  ;;  (bytes->hex-string (h-encode #"www.example.com"))
  ;;  (h-decode (bytes #xf1 #xe3 #xc2 #xe5 #xf2 #x3a #x6b #xa0 #xab #x90 #xf4 #xff))

  (void))
