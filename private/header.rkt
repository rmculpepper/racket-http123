#lang racket/base
(require racket/class
         racket/match
         racket/list
         "interfaces.rkt"
         "regexp.rkt")
(provide (all-defined-out))

;; References:
;; - HTTP/1.1: https://tools.ietf.org/html/rfc7230

(define headers%
  (class* object% ()
    (init-field raw-headers) ;; (Listof Bytes)
    (super-new)

    (define headers (make-hasheq)) ;; Hasheq[Symbol => (U Bytes (Listof Bytes))]
    (let ()
      (define list-valued (make-hasheq))
      (for ([raw-header (in-list raw-headers)])
        (cond [(regexp-match (rx (rx ^ HEADER $)) raw-header)
               => (match-lambda
                    [(list _ key-bs value-bs)
                     (define key (normalize-key key-bs))
                     (cond [(hash-ref headers key #f)
                            => (lambda (old-v)
                                 (define old-v* (if (bytes? old-v) (list old-v) old-v))
                                 (define new-v (cons value-bs old-v*))
                                 (hash-set! headers key new-v)
                                 (hash-set! list-valued key #t))]
                           [else
                            (hash-set! headers key value-bs)])])]
              [else (s-error "malformed header\n  header: ~e" raw-header #:code 'bad-header)]))
      (for ([k (in-hash-keys list-valued)])
        (define vs (reverse (hash-ref headers k)))
        (case k
          [(set-cookie)
           ;; Set-Cookie is just strange; see RFC 7230 Section 3.2.2 (Note).
           (hash-set! headers k vs)]
          [else
           ;; Otherwise, pre-split components on commas.
           (hash-set! headers k (apply append (map split-on-commas vs)))])))

    ;; FIXME: instead of pre-splitting, maybe make a list of known List-Valued headers?

    (define/public (get-raw-headers) raw-headers)
    (define/public (get-headers) headers)

    (define/public (normalize-key key)
      (cond [(symbol? key) key]
            [(string? key) (string->symbol (string-downcase key))]
            [(bytes? key) (normalize-key (bytes->string/latin-1 key))]
            [else (error 'normalize-key "bad key: ~e" key)]))

    (define/public (has-key? key)
      (hash-has-key? headers (normalize-key key)))

    (define/public (get-values key [default #f])
      (define v (hash-ref headers (normalize-key key) #f))
      (cond [(list? v) v] ;; elements are already comma-split
            [(bytes? v) (split-on-commas v)]
            [else (if (procedure? default) (default) default)]))

    (define/public (get-value key [default #f])
      (define v (hash-ref headers (normalize-key key) #f))
      (cond [(list? v) (apply bytes-append (add-between v #", "))]
            [(bytes? v) v]
            [else (if (procedure? default) (default) default)]))

    (define/public (get-string-value/latin-1 key)
      (define v (get-value key #f))
      (and v (bytes->string/latin-1 v)))

    (define/public (get-string-values/latin-1 key)
      (define vs (get-values key #f))
      (and vs (map bytes->string/latin-1 vs)))

    (define/public (get-nat-value key) ;; -> Nat or #f, never raises error (FIXME: pre-check!)
      (bytes->nat (get-value key #f)))

    (define/public (has-value? key value)
      (equal? (get-value (normalize-key key) #f) value))
    (define/public (has-values? key values)
      (equal? (get-values (normalize-key key) #f) values))

    (define/public (check-value key predicate [description #f])
      (define v (get-value key #f))
      (unless (predicate v)
        (s-error "bad header value\n  header: ~e\n  expected: ~a\n  got: ~e"
                 key (or description (object-name predicate)) v
                 #:code 'bad-header-value)))

    (define/public (check-values key predicate [description #f])
      (define vs (get-values key #f))
      (unless (and vs (andmap predicate vs))
        (s-error "bad header values\n  header: ~e\n  expected: ~a\n  got: ~e"
                 key (or description (object-name predicate)) vs
                 #:code 'bad-header-value)))

    ))


;; ============================================================
;; Header (Request)

;; A HeaderSet is (Listof Bytes).

(define (headerset-missing? hs rx)
  (not (headerset-has? hs rx)))

(define (headerset-has? hs key)
  (and (headerset-select hs key) #t))

(define (headerset-select hs key)
  (define rx
    (case key
      [(Connection) #rx#"^(?i:Connection):[ \t]$"]
      [(Content-Length) #rx#"^(?i:Content-Length):[ \t]$"]
      [(Transfer-Encoding) #rx#"^(?i:Transfer-Encoding):[ \t]$"]
      [else
       (cond [(regexp? key) key]
             [else (let ([key (if (string? key) key (format "~a" key))])
                     (format "^(?i:~a):[ \t]$" (regexp-quote key)))])]))
  (for/or ([h (in-list hs)]) (and (regexp-match rx h) h)))

;; Reference: https://tools.ietf.org/html/rfc7230#appendix-B

(provide OWS TCHAR TOKEN)

(define-rx FIELD-VCHAR "[\x21-\x7E]")
(define-rx FIELD-CONTENT (rx FIELD-VCHAR (? (+ "[ \t]" FIELD-VCHAR))))
(define-rx FIELD-VALUE (* FIELD-CONTENT))

(define-rx HEADER-START (rx (record TOKEN) ":"))
(define-rx HEADER (rx HEADER-START OWS (record FIELD-VALUE) OWS))

(define (split-on-commas bs)
  (regexp-split #rx#"[ \t]*,[ \t]*" bs))

;; FIXME: add reasonable limits for eg, Content-Length headers
;; FIXME: pre-scan headers for reasonable values?

;; ------------------------------------------------------------
;; Misc Bytes Utils

(define (bytes->symbol bs)
  (string->symbol (bytes->string/latin-1 bs)))

(define (bytes->nat v) ;; Bytes -> Nat or #f
  (define s (and v (bytes->string/latin-1 v)))
  (define n (and s (string->number s)))
  (and (exact-nonnegative-integer? n) n))

(define (bytes-join bss sep)
  (apply bytes-append (add-between bss sep)))
