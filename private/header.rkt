#lang racket/base
(require racket/class
         racket/contract/base
         racket/match
         racket/list
         racket/symbol
         racket/struct
         "interfaces.rkt"
         "regexp.rkt")
(provide (all-defined-out))

;; References:
;; - HTTP/1.1: https://tools.ietf.org/html/rfc7230

(define-rx TOKEN+ (rx TOKEN (* OWS "," OWS TOKEN)))

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
;; Header key forms

(define (header-key-symbol? v)
  (and (symbol? v)
       (or (common-symbol->bytes v)
           (header-key-name? (symbol->immutable-string v)))
       #t))

;; header-key->symbol : HeaderKey -> Symbol, or #f if fail-ok?
(define (header-key->symbol key [fail-ok? #f])
  (cond [(hash-ref common-bytes=>symbol key #f) => values]
        [(header-key-symbol? key) key]
        [(and (bytes? key) (header-key-name-ci? key))
         (string->symbol (string-downcase (bytes->string/latin-1 key)))]
        [(and (string? key) (header-key-name-ci? key))
         (string->symbol (string-downcase key))]
        [fail-ok? #f]
        [else (error 'header-key->symbol "bad header key\n  key: ~e" key)]))

;; header-key->bytes : HeaderKey -> Bytes, or #f if fail-ok?
(define (header-key->bytes key0 [fail-ok? #f])
  (let loop ([key key0])
    (cond [(symbol? key) (or (common-symbol->bytes key)
                             (loop (symbol->immutable-string key)))]
          [(and (string? key) (header-key-name-ci? key))
           (string->bytes/latin-1 (string-downcase key))]
          [(and (bytes? key) (header-key-name? key)) key]
          [(and (bytes? key) (header-key-name-ci? key))
           (string->bytes/latin-1 (string-downcase (bytes->string/latin-1 key)))]
          [fail-ok? #f]
          [else (error 'header-key->bytes "bad header key\n  key: ~e" key0)])))

(define (header-key-name? s [pseudo-ok? #t])
  (or (regexp-match? (rx^$ lower-TOKEN) s)
      (and pseudo-ok?
           (regexp-match? #rx"^:(?:authority|method|path|scheme|status)$" s))))

(define (header-key-name-ci? s [pseudo-ok? #t])
  (or (regexp-match (rx^$ TOKEN) s)
      (and pseudo-ok?
           (regexp-match #rx#"^:(?i:authority|method|path|scheme|status)$" s))))

;; based on static table of QPACK (for HTTP/3) draft 21
(define (common-symbol->bytes sym)
  (case sym
    [(:authority)                       #":authority"]
    [(:method)                          #":method"]
    [(:path)                            #":path"]
    [(:scheme)                          #":scheme"]
    [(:status)                          #":status"]
    [(accept)                           #"accept"]
    [(accept-encoding)                  #"accept-encoding"]
    [(accept-language)                  #"accept-language"]
    [(accept-ranges)                    #"accept-ranges"]
    [(access-control-allow-credentials) #"access-control-allow-credentials"]
    [(access-control-allow-headers)     #"access-control-allow-headers"]
    [(access-control-allow-methods)     #"access-control-allow-methods"]
    [(access-control-allow-origin)      #"access-control-allow-origin"]
    [(access-control-expose-headers)    #"access-control-expose-headers"]
    [(access-control-request-headers)   #"access-control-request-headers"]
    [(access-control-request-method)    #"access-control-request-method"]
    [(age)                              #"age"]
    [(alt-svc)                          #"alt-svc"]
    [(authorization)                    #"authorization"]
    [(cache-control)                    #"cache-control"]
    [(content-disposition)              #"content-disposition"]
    [(content-encoding)                 #"content-encoding"]
    [(content-length)                   #"content-length"]
    [(content-security-policy)          #"content-security-policy"]
    [(content-type)                     #"content-type"]
    [(cookie)                           #"cookie"]
    [(date)                             #"date"]
    [(early-data)                       #"early-data"]
    [(etag)                             #"etag"]
    [(expect-ct)                        #"expect-ct"]
    [(forwarded)                        #"forwarded"]
    [(if-modified-since)                #"if-modified-since"]
    [(if-none-match)                    #"if-none-match"]
    [(if-range)                         #"if-range"]
    [(last-modified)                    #"last-modified"]
    [(link)                             #"link"]
    [(location)                         #"location"]
    [(origin)                           #"origin"]
    [(purpose)                          #"purpose"]
    [(range)                            #"range"]
    [(referer)                          #"referer"]
    [(server)                           #"server"]
    [(set-cookie)                       #"set-cookie"]
    [(strict-transport-security)        #"strict-transport-security"]
    [(timing-allow-origin)              #"timing-allow-origin"]
    [(upgrade-insecure-requests)        #"upgrade-insecure-requests"]
    [(user-agent)                       #"user-agent"]
    [(vary)                             #"vary"]
    [(x-content-type-options)           #"x-content-type-options"]
    [(x-forwarded-for)                  #"x-forwarded-for"]
    [(x-frame-options)                  #"x-frame-options"]
    [(x-xss-protection)                 #"x-xss-protection"]
    [else #f]))

(define common-bytes=>symbol
  #hash((#":authority"                       . :authority)
        (#":method"                          . :method)
        (#":path"                            . :path)
        (#":scheme"                          . :scheme)
        (#":status"                          . :status)
        (#"accept"                           . accept)
        (#"accept-encoding"                  . accept-encoding)
        (#"accept-language"                  . accept-language)
        (#"accept-ranges"                    . accept-ranges)
        (#"access-control-allow-credentials" . access-control-allow-credentials)
        (#"access-control-allow-headers"     . access-control-allow-headers)
        (#"access-control-allow-methods"     . access-control-allow-methods)
        (#"access-control-allow-origin"      . access-control-allow-origin)
        (#"access-control-expose-headers"    . access-control-expose-headers)
        (#"access-control-request-headers"   . access-control-request-headers)
        (#"access-control-request-method"    . access-control-request-method)
        (#"age"                              . age)
        (#"alt-svc"                          . alt-svc)
        (#"authorization"                    . authorization)
        (#"cache-control"                    . cache-control)
        (#"content-disposition"              . content-disposition)
        (#"content-encoding"                 . content-encoding)
        (#"content-length"                   . content-length)
        (#"content-security-policy"          . content-security-policy)
        (#"content-type"                     . content-type)
        (#"cookie"                           . cookie)
        (#"date"                             . date)
        (#"early-data"                       . early-data)
        (#"etag"                             . etag)
        (#"expect-ct"                        . expect-ct)
        (#"forwarded"                        . forwarded)
        (#"if-modified-since"                . if-modified-since)
        (#"if-none-match"                    . if-none-match)
        (#"if-range"                         . if-range)
        (#"last-modified"                    . last-modified)
        (#"link"                             . link)
        (#"location"                         . location)
        (#"origin"                           . origin)
        (#"purpose"                          . purpose)
        (#"range"                            . range)
        (#"referer"                          . referer)
        (#"server"                           . server)
        (#"set-cookie"                       . set-cookie)
        (#"strict-transport-security"        . strict-transport-security)
        (#"timing-allow-origin"              . timing-allow-origin)
        (#"upgrade-insecure-requests"        . upgrade-insecure-requests)
        (#"user-agent"                       . user-agent)
        (#"vary"                             . vary)
        (#"x-content-type-options"           . x-content-type-options)
        (#"x-forwarded-for"                  . x-forwarded-for)
        (#"x-frame-options"                  . x-frame-options)
        (#"x-xss-protection"                 . x-xss-protection)))

;; ============================================================
;; Misc Bytes Utils

(define (bytes->symbol bs)
  (string->symbol (bytes->string/latin-1 bs)))

(define (bytes->nat v) ;; Bytes -> Nat or #f
  (define s (and v (bytes->string/latin-1 v)))
  (define n (and s (string->number s)))
  (and (exact-nonnegative-integer? n) n))

(define (bytes-join bss sep)
  (apply bytes-append (add-between bss sep)))

(define (symbol->bytes s)
  (string->bytes/utf-8 (symbol->string s)))

;; ============================================================

(define headers<%>
  (interface ()
    [get-headers
     (->m (hash/c header-key-symbol? (or/c bytes? (listof bytes?))))]
    [get-headers/pairs
     (->m (listof (cons/c bytes? bytes?)))]
    [get-headers/lines
     (->m (listof bytes?))]
    [has-key?
     (->m header-key-symbol? boolean?)]
    [get-values
     (->m header-key-symbol? (or/c (listof bytes?) #f))]
    [get-value
     (->m header-key-symbol? (or/c bytes? #f))]
    [get-ascii-string
     (->m header-key-symbol? (or/c string? #f))]
    [get-integer-value
     (->m header-key-symbol? (or/c exact-integer? #f))]
    [has-value?
     (->m header-key-symbol? bytes? boolean?)]
    [value-matches?
     (->*m [header-key-symbol? regexp?] [#:exact? boolean?] boolean?)]
    ))

;; ----------------------------------------

(define headers%
  (class* object% (headers<%> printable<%>)
    (init-field [headers (make-hasheq)] ;; Hasheq[Symbol => (U Bytes (Listof Bytes))]
                [party #f])
    (super-new)

    (define/public (get-headers) headers)

    (define/public (get-headers/pairs [combine? #t])
      (for*/list ([(k vs) (in-hash headers)]
                  [v (in-list (if (list? vs)
                                  (if combine? (list (bytes-join vs #", ")) vs)
                                  (list vs)))])
        (cons (header-key->bytes k) v)))

    (define/public (get-headers/lines [combine? #t])
      (for/list ([p (in-list (get-headers/pairs combine?))])
        (match-define (cons k v) p)
        (bytes-append k #": " v)))

    (define/public (has-key? key)
      (hash-has-key? headers key))

    (define/public (get-values key)
      (define vs (hash-ref headers key #f))
      (cond [(list? vs) vs]
            [(bytes? vs) (list vs)]
            [else #f]))

    ;; Warning about automatically joining list-valued headers:
    ;; Set-Cookie is just strange; see RFC 7230 Section 3.2.2 (Note).
    (define/public (get-value key)
      (define v (hash-ref headers key #f))
      (cond [(list? v) (bytes-join v #", ")]
            [(bytes? v) v]
            [else #f]))

    (define/public (get-ascii-string key)
      (define v (get-value key #f))
      (and v (regexp-match? #px#"^[[:ascii:]]*$" v) (bytes->string/latin-1 v)))

    (define/public (get-integer-value key) ;; -> Int or #f
      (define v (get-ascii-string key))
      (and v (let ([n (string->number v)]) (and (exact-integer? n)))))

    (define/public (has-value? key value)
      (equal? (get-value key) value))

    (define/public (value-matches? key rx #:exact? [exact? #t])
      (define v (get-value key))
      (and v (if exact? (regexp-match-exact? rx v) (regexp-match? rx v))))

    (define/public (check-value key predicate [description #f])
      (define v (get-value key))
      (unless (predicate v)
        (http-error "bad header value\n  header: ~e\n  expected: ~a\n  got: ~e"
                    key (or description (object-name predicate)) v
                    #:party party #:code 'bad-header-value)))

    ;; ----

    (define/public (custom-write out)
      (headers-printer this out #t))
    (define/public (custom-print out mode)
      (custom-write out))
    (define/public (custom-display out)
      (custom-write out))
    ))

(define headers-printer
  (make-constructor-style-printer
   (lambda (self) 'headers)
   (lambda (self) (list (hash-map (send self get-headers) list)))))

;; ------------------------------------------------------------

(define (make-headers-from-list raw-headers get-kv #:party [party #f])
  (define headers (make-hasheq))
  (define list-valued (make-hasheq))
  (for ([raw-header (in-list raw-headers)])
    (define-values (key-bs val-bs) (get-kv raw-header))
    (define key (or (header-key->symbol key-bs #t)
                    (http-error "bad header key\n  key: ~e" key-bs
                                #:who 'make-headers-from-list #:party party)))
    (cond [(hash-ref headers key #f)
           => (lambda (old-v)
                (define old-v* (if (bytes? old-v) (list old-v) old-v))
                (define new-v (cons val-bs old-v*))
                (hash-set! headers key new-v)
                (hash-set! list-valued key #t))]
          [else (hash-set! headers key val-bs)]))
  (for ([k (in-hash-keys list-valued)])
    (hash-set! headers k (reverse (hash-ref headers k))))
  (new headers% (headers headers) (party party)))

;; make-headers-from-lines : (Listof Bytes) -> headers%
(define (make-headers-from-lines raw-headers #:party [party #f])
  (make-headers-from-list
   raw-headers
   (lambda (raw-header)
     (match (regexp-match (rx^$ HEADER) raw-header)
       [(list _ key-bs val-bs) (values key-bs val-bs)]
       [_ (http-error "malformed header line\n  line: ~e" raw-header
                      #:who 'make-headers-from-lines #:code 'bad-header-line
                      #:party party)]))))

(define (make-headers-from-lists raw-headers #:party [party #f])
  (make-headers-from-list raw-headers (lambda (e) (values (car e) (cadr e)))))
