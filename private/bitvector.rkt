#lang racket/base
(require (for-syntax racket/base)
         racket/match)
(provide (all-defined-out))

;; A ShortBitVector is a nonnegative exact integer
;; where [b0 b1 ...] is encoded as (SUM_k bk*2^k)<<16 + LEN

;; That is, the bits are the little-endian interpretation of the
;; prefix of the number (independent of the hardware endianness).
;; Example: (make-sbv #b1011 4) represents [1 1 0 1]

;; Bitvectors up to 2^16-1 bits are representable.
;; Bitvectors up to about 46 bits are representable by fixnums.

;; ============================================================
;; Operations on short bitvector encodings

(define SBVLENBITS 16)
(define SBVLENBOUND (expt 2 SBVLENBITS))
(define SBVLENMASK (sub1 SBVLENBOUND))

(define (make-sbv n len)
  (if (< len SBVLENBOUND)
      (bitwise-ior (arithmetic-shift n SBVLENBITS) len)
      (error 'make-sbv "too long\n  length: ~e" len)))

(define (make-be-sbv n len)
  (sbv-reverse (make-sbv n len)))

(define empty-sbv (make-sbv 0 0))
(define (sbv-empty? sbv) (eqv? sbv empty-sbv))

(define (sbv-length sbv)
  (bitwise-and sbv SBVLENMASK))

(define (sbv-bits sbv)
  (arithmetic-shift sbv (- SBVLENBITS)))
(define (sbv-bit-field sbv start end)
  ;; ok for start, end to be past end of bitvector (zero)
  (bitwise-bit-field sbv (+ start SBVLENBITS) (+ end SBVLENBITS)))
(define (sbv-shift sbv lshift)
  (make-sbv (arithmetic-shift (sbv-bits sbv) lshift)
            (max 0 (+ (sbv-length sbv) lshift))))

(define (sbv-ref sbv k)
  (if (sbv-bit-set? sbv k) 1 0))

(define (sbv-bit-set? sbv k)
  (bitwise-bit-set? sbv (+ k SBVLENBITS)))

(define (sbv-car sbv) (sbv-ref sbv 0))
(define (sbv-cdr sbv) (sbv-shift sbv -1))

(define (sbv-cons bit sbv)
  (make-sbv (bitwise-ior (arithmetic-shift (sbv-bits sbv) 1) bit)
            (add1 (sbv-length sbv))))

(define (sbv-append2 a b)
  (define alen (sbv-length a))
  (define blen (sbv-length b))
  (make-sbv (bitwise-ior (sbv-bits a) (arithmetic-shift (sbv-bits b) alen))
            (+ alen blen)))

(define sbv-append
  (case-lambda
    [() empty-sbv]
    [(a) a]
    [(a b) (sbv-append2 a b)]
    [as (foldr sbv-append2 empty-sbv as)]))

(define (sbv-reverse sbv)
  (sbv-reverse/byte-loop sbv))

(define (sbv-reverse/bit-loop sbv)
  ;; This version is slow even for fixnum arguments!
  (define len (sbv-length sbv))
  (define bits (sbv-bits sbv))
  (make-sbv (let loop ([i 0] [acc 0])
              (if (< i len)
                  (loop (add1 i)
                        (bitwise-ior (if (bitwise-bit-set? bits i) 1 0)
                                     (arithmetic-shift acc 1)))
                  acc))
            len))

(define (sbv-reverse/byte-loop sbv)
  (define len (sbv-length sbv))
  (let loop ([i 0] [acc 0])
    (cond [(< i len)
           (loop (+ i 8)
                 (bitwise-ior (reverse-byte (sbv-bit-field sbv i (+ i 8)))
                              (arithmetic-shift acc 8)))]
          [else ;; may have overshot
           (make-sbv (arithmetic-shift acc (- len i)) len)])))

(define (reverse-byte byte)
  (define-syntax (quote-table stx)
    (define (revbyte n)
      (let loop ([i 0] [acc 0])
        (if (< i 8)
            (loop (add1 i)
                  (bitwise-ior (if (bitwise-bit-set? n i) 1 0) (arithmetic-shift acc 1)))
            acc)))
    #`(quote #,(apply bytes (for/list ([i (in-range 256)]) (revbyte i)))))
  (bytes-ref (quote-table) byte))

(define (sbv-prefix? a b) ;; Is a prefix of b?
  (and (<= (sbv-length a) (sbv-length b))
       (= (sbv-bits a) (sbv-bit-field b 0 (sbv-length a)))))

(define (sbv-split sbv n)
  (define len (sbv-length sbv))
  (define n* (min n len))
  (values (make-sbv (sbv-bit-field sbv 0 n*) n*)
          (make-sbv (sbv-bit-field sbv n* len) (- len n*))))

(define (sbv-map f sbv)
  (for/list ([i (in-range (sbv-length sbv))])
    (f (sbv-ref sbv i))))

(define (sbv->string sbv)
  (apply string (sbv-map (lambda (bit) (case bit [(0) #\0] [(1) #\1])) sbv)))


;; ============================================================
;; Bitbuffers (output)

;; A bitbuffer is (bitbuffer ByteOutputPort ShortBitvector Nat)
(struct bitbuffer (out partial start) #:mutable)

(define (make-bitbuffer [start 0])
  (let ([start (modulo start 8)])
    (bitbuffer (open-output-bytes) (make-sbv 0 start) start)))

(define (bitbuffer-write-bit bb bit)
  (bitbuffer-write-sbv bb (make-sbv bit 1)))

(define (bitbuffer-write-sbv bb sbv)
  (match-define (bitbuffer out partial start) bb)
  (define partial* (sbv-append partial sbv))
  ;;(eprintf "--> ~a\n" (sbv->string sbv))
  (set-bitbuffer-partial! bb (-flush-partial partial* out)))

(define (-flush-partial partial out)
  (define len (sbv-length partial))
  (define blen (quotient len 8))
  (for ([i (in-range blen)])
    (define flush-byte (sbv-bit-field partial (* i 8) (* (add1 i) 8)))
    ;;(eprintf "flush: ~a\n" (sbv->string (make-sbv flush-byte 8)))
    (define flush-byte* (reverse-byte flush-byte))
    ;;(eprintf "<<< ~a~a\n" (make-string (* i 8) #\space)
    ;;         (substring (number->string (+ 1024 flush-byte*) 2) 3))
    (write-byte flush-byte* out))
  (sbv-shift partial (* blen -8)))

;; Returns bytes, start bit index, and end bit index.
(define (bitbuffer-get-bytes bb #:reset? [reset? #f] #:pad [padwith #x00])
  (match-define (bitbuffer out partial start) bb)
  (define padlen (bitbuffer-pad bb padwith))
  (define bs (get-output-bytes out reset?))
  (define end (- (* 8 (bytes-length bs)) padlen))
  (unless (or reset? (zero? padlen))
    (file-position out (sub1 (file-position out))))
  (values bs start end))

(define (bitbuffer-pad bb [padwith #x00])
  (match-define (bitbuffer out partial start) bb)
  ;;(eprintf "padding partial: ~a\n" (sbv->string partial))
  (define len (sbv-length partial))
  (define padlen (- (* (quotient (+ len 7) 8) 8) len))
  ;;(eprintf "pad: partial=~s=~a, len=~s, padlen=~s\n" partial (sbv->string partial) len padlen)
  (-write-subbyte bb padwith 0 padlen)
  (set-bitbuffer-partial! bb empty-sbv)
  padlen)

(define (-write-subbyte bb byte start-biti end-biti)
  (bitbuffer-write-sbv bb
                       (make-sbv (bitwise-bit-field byte start-biti end-biti)
                                 (- end-biti start-biti))))

#;
(define (bitbuffer-get-bitvector bb #:reset? [reset? #f])
  (define (get-bitvector)
    (define padlen (bitbuffer-pad bb))
    (match-define (bitbuffer out partial) bb)
    (define bs (get-output-bytes out reset?))
    (long-bitvector bs (- (* 8 (bytes-length bs)) padlen)))
  (cond [reset? (get-bitvector)]
        [else
         (match-define (bitbuffer out saved-partial) bb)
         (define saved-pos (file-position out))
         (begin0 (get-bitvector)
           (file-position out saved-pos)
           (set-bitbuffer-partial! bb saved-partial))]))

;; input?

(define (bytes-be-bit-set? bs biti)
  (define bytei (quotient biti 8))
  (define biti-in-byte (remainder biti 8))
  (define b (bytes-ref bs bytei))
  (bitwise-bit-set? b (- 7 biti-in-byte)))

#|
;; An input-bitport is (input-bitport port bv)
(struct input-bitport (in cache) #:mutable #:transparent)
(define (-input-cache-bits bin nbits)
  (match-define (input-bitport in cache) bin)
  (cond [(< (sbv-length cache) nbits)
         (define next (read-byte in))
         (cond [(byte? next)
                (define cache* (bv-append cache (make-sbv next 8)))
                (set-input-bitport-cache! bin cache*)]
               [else cache])]
        [else cache]))
(define (read-bit bin)
  (define bv (read-bv 1 bin))
  (cond [(eof-object? bv) eof]
        [else (bv-car bv)]))
(define (read-bv n bin)
  (define cache (-input-cache-bits bin 1))
  (cond [(bv-empty? cache) eof]
        [else
         (define-values (bv cache*) (bv-split cache n))
         (set-input-bitport-cache! bin cache*)
         bv]))
|#

;; ============================================================

(module+ main
  (define rand-sbvs
    (for/vector ([i #e1e5])
      (define len (add1 (random 16)))
      (add1 (random (expt 2 len)))))

  (printf "length\n")
  (time (for ([i (in-range 1000)])
          (for ([sbv (in-vector rand-sbvs)])
            (sbv-length sbv))))

  (printf "cdr\n")
  (time (for ([i (in-range 1000)])
          (for ([sbv (in-vector rand-sbvs)])
            (sbv-cdr sbv))))

  (printf "cons\n")
  (time (for ([i (in-range 1000)])
          (for ([sbv (in-vector rand-sbvs)])
            (sbv-cons 1 sbv))))

  (printf "ref 0\n")
  (time (for ([i (in-range 1000)])
          (for ([sbv (in-vector rand-sbvs)])
            (sbv-ref sbv 0))))

  (printf "reverse\n")
  (time (for ([i (in-range 1)])
          (for ([sbv (in-vector rand-sbvs)])
            (sbv-reverse sbv))))
  )
