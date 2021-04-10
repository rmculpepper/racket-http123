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

(define (header-key-symbol? v)
  (and (symbol? v)
       (or (and (common-symbol->bytes v) #t)
           (header-key-name? (symbol->immutable-string v)))))

;; ============================================================
;; Headers object

;; Used to represent response headers.

;; A Headers is an instance of headers<%>.

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
     (->*m [header-key-symbol? byte-regexp?] [#:exact? boolean?] boolean?)]
    ))

;; ----------------------------------------

(define headers%
  (class* object% (headers<%> class-printable<%>)
    (init-field [headers (make-hasheq)]) ;; Hasheq[Symbol => (U Bytes (Listof Bytes))]
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
      (define v (get-value key))
      (and v (regexp-match? #px#"^[[:ascii:]]*$" v) (bytes->string/latin-1 v)))

    (define/public (get-integer-value key) ;; -> Int or #f
      (define v (get-ascii-string key))
      (and v (let ([n (string->number v)]) (and (exact-integer? n) n))))

    (define/public (has-value? key value)
      (equal? (get-value key) value))

    (define/public (value-matches? key rx #:exact? [exact? #t])
      (define v (get-value key))
      (and v (if exact? (regexp-match-exact? rx v) (regexp-match? rx v))))

    (define/public (check-value key predicate [description #f])
      (define v (get-value key))
      (unless (predicate v)
        (error* "bad header value\n  header: ~e\n  expected: ~a\n  got: ~e"
                key (or description (object-name predicate)) v
                #:code 'bad-header-value)))

    (define/public (remove! key)
      (hash-remove! headers key))

    ;; ----

    (define/public (get-printing-classname)
      'headers%)
    (define/public (get-printing-components)
      (values '(headers) (list headers) #f))
    ))

;; ----------------------------------------

;; make-headers-from-list : (Listof X) (X -> (values HeaderKey HeaderValueBytes))
;;                       -> headers%
(define (make-headers-from-list raw-headers get-kv)
  (define headers (make-hasheq))
  (define list-valued (make-hasheq))
  (for ([raw-header (in-list raw-headers)])
    (define-values (key-bs val-bs) (get-kv raw-header))
    (define key (or (header-key->symbol key-bs #t)
                    (error* "bad header key\n  key: ~e" key-bs
                            #:who 'make-headers-from-list #:code 'bad-header-key)))
    (cond [(hash-ref headers key #f)
           => (lambda (old-v)
                (define old-v* (if (bytes? old-v) (list old-v) old-v))
                (define new-v (cons val-bs old-v*))
                (hash-set! headers key new-v)
                (hash-set! list-valued key #t))]
          [else (hash-set! headers key val-bs)]))
  (for ([k (in-hash-keys list-valued)])
    (hash-set! headers k (reverse (hash-ref headers k))))
  (new headers% (headers headers)))

;; make-headers-from-lines : (Listof Bytes) -> headers%
(define (make-headers-from-lines raw-headers)
  (make-headers-from-list
   raw-headers
   (lambda (raw-header)
     (match (regexp-match (rx^$ HEADER) raw-header)
       [(list _ key-bs val-bs) (values key-bs val-bs)]
       [_ (error* "malformed header line\n  line: ~e" raw-header
                  #:who 'make-headers-from-lines #:code 'bad-header-line)]))))


;; A HeaderEntry is (list Bytes Bytes) | (list Bytes Bytes 'never-add)

;; make-headers-from-entries : (Listof HeaderEntry) -> headers%
;; FIXME: preserve 'never-add ??
(define (make-headers-from-entries entries)
  (make-headers-from-list entries check-header-entry))

;; check-header-entry : Any -> (values HeaderKeyBytes HeaderValueBytes)
(define (check-header-entry entry)
  (match entry
    [(list* (? bytes? (and (or (regexp (rx^$ lower-TOKEN)) #":status") key))
            (? bytes? (and (regexp (rx^$ FIELD-VALUE)) value))
            (? (lambda (v) (member v '((never-add) ())))))
     (values key value)]
    [h (error* "malformed header entry\n  header: ~e" h)]))


;; ============================================================
;; HeaderLines

;; Used to represent request headers (for http11)

;; A FlexibleHeaderList is (Listof FlexibleHeader).
;; A FlexibleHeader is one of
;; - (list FlexibleHeaderKey FlexibleHeaderValue)
;; - Bytes                  -- matching HEADER
;; - String                 -- matching HEADER
;; FlexibleHeaderKey = Symbol | Bytes | String  -- matching TOKEN
;; FlexibleHeaderValue = String | Bytes         -- matching OWS FIELD-VALUE OWS

;; A HeaderLines = (Listof Bytes)

;; normalize-headerlines : FlexibleHeaderList -> HeaderLines
;; Normalizing involves checking and converting to bytes, not downcasing keys.
(define (normalize-headerlines hs)
  (map normalize-header hs))
(define (normalize-header h)
  (match h
    [(list key value)
     (bytes-append (normalize-key key) #": " (normalize-value value))]
    [(? bytes)
     (cond [(regexp-match-exact? (rx HEADER) h) h]
           [else (error 'normalize-header "bad header line\n  header: ~e" h)])]
    [(? string?)
     (cond [(regexp-match-exact? (rx HEADER) h) (string->bytes/latin-1 h)]
           [else (error 'normalize-header "bad header line\n  header: ~e" h)])]))
(define (normalize-key key)
  (match key
    [(? bytes? (regexp (rx^$ TOKEN))) key]
    [(? string? (regexp (rx^$ TOKEN))) (string->bytes/latin-1 key)]
    [else (error 'normalize-header "bad header key\n  key: ~e" key)]))
(define (normalize-value value)
  (match value
    [(? string? (regexp (rx^$ (rx OWS (record FIELD-VALUE) OWS))
                        (list _ field-value)))
     (string->bytes/latin-1 field-value)]
    [(? bytes? (regexp (rx^$ (rx OWS (record FIELD-VALUE) OWS))
                       (list _ field-value)))
     field-value]
    [_ (error 'normalize-header "bad header value\n  value: ~e" value)]))

(define (headerlines-missing? hs rx)
  (not (for/or ([h (in-list hs)])
         (regexp-match? rx h))))


;; ============================================================
;; Header syntax utils

;; Reference: https://tools.ietf.org/html/rfc7230#appendix-B

(provide OWS TCHAR TOKEN)

(define-rx FIELD-VCHAR "[\x21-\x7E]")
(define-rx FIELD-CONTENT (rx FIELD-VCHAR (? (+ "[ \t]" FIELD-VCHAR))))
(define-rx FIELD-VALUE (* FIELD-CONTENT))

(define-rx HEADER-START (rx (record TOKEN) ":"))
(define-rx HEADER (rx HEADER-START OWS (record FIELD-VALUE) OWS))

(define-rx TOKEN+ (rx TOKEN (* OWS "," OWS TOKEN)))


;; ============================================================
;; Header key forms

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

(define (header-key-name? s)
  (regexp-match? (rx^$ lower-TOKEN) s))

(define (header-key-name-ci? s)
  (regexp-match (rx^$ TOKEN) s))

(define (pseudo-header-key-name? bs)
  (regexp-match? #rx#"^:(?:authority|method|path|scheme|status)$" bs))

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

(define reserved-headers
  '(host
    accept-encoding ;; FIXME
    content-length
    connection
    keep-alive
    upgrade
    transfer-encoding
    te
    trailer))

(define reserved-headerline-rxs
  '(#rx#"^(?i:Host):"
    #rx#"^(?i:Accept-Encoding):" ;; In principle, this belongs to a separate layer...
    #rx#"^(?i:Content-Length):"
    #rx#"^(?i:Connection|Keep-Alive|Upgrade):"
    #rx#"^(?i:Transfer-Encoding|TE|Trailer):"))
