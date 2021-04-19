#lang racket/base
(require racket/class
         racket/contract/base
         racket/match
         racket/list
         racket/symbol
         "interfaces.rkt"
         "regexp.rkt"
         "util.rkt")
(provide (all-defined-out))

;; References:
;; - HTTP/1.1: https://tools.ietf.org/html/rfc7230

(define (header-key-symbol? v)
  (and (symbol? v)
       (or (and (common-symbol->bytes v) #t)
           (header-key-name? (symbol->immutable-string v)))))

;; ============================================================
;; Header object

;; A Header is an instance of header<%>.
;; Used to represent response header (and trailer).

;; Notes on terminology used by RFC 7230:
;; - header - for entire thing (not "headers")
;; - header field - one line / mapping
;; - field-name - left-hand side -- we use "key" instead
;; - field-value - right-hand side

(define header<%>
  (interface ()
    [get-header-table
     (->m (hash/c symbol? (or/c bytes? (listof bytes?))))]
    [get-header-entries
     (->*m [] [boolean?] (listof (list/c bytes? bytes?)))]
    [get-header-lines
     (->*m [] [boolean?] (listof bytes?))]
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

(define header%
  (class* object% (header<%> class-printable<%>)
    (init-field [table (make-hasheq)]) ;; Hasheq[Symbol => (U Bytes (Listof Bytes))]
    (super-new)

    (define/public (get-header-table)
      (hash-copy table))

    (define/public (get-header-entries [combine? #f])
      (for*/list ([(k vs) (in-hash table)]
                  [v (in-list (if (list? vs)
                                  (if combine? (list (bytes-join vs #", ")) vs)
                                  (list vs)))])
        (list (header-key->bytes k) v)))

    (define/public (get-header-lines [combine? #f])
      (for/list ([p (in-list (get-header-entries combine?))])
        (match-define (list k v) p)
        (bytes-append k #": " v)))

    (define/public (has-key? key)
      (hash-has-key? table key))

    (define/public (get-values key)
      (define vs (hash-ref table key #f))
      (cond [(list? vs) vs]
            [(bytes? vs) (list vs)]
            [else #f]))

    ;; Warning about automatically joining list-valued headers:
    ;; Set-Cookie is just strange; see RFC 7230 Section 3.2.2 (Note).
    (define/public (get-value key)
      (define v (hash-ref table key #f))
      (cond [(list? v) (bytes-join v #", ")]
            [(bytes? v) v]
            [else #f]))

    (define/public (get-ascii-string key)
      (define v (get-value key))
      (and v (ascii-bytes? v) (bytes->string/ascii v)))

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
        (h-error "bad header field value\n  key: ~e\n  expected: ~a\n  got: ~e"
                 key (or description (object-name predicate)) v
                 #:info (hasheq 'code 'bad-header-field-value))))

    (define/public (remove! key)
      (hash-remove! table key))

    ;; ----

    (define/public (get-printing-classname)
      'header%)
    (define/public (get-printing-components)
      (values '(table) (list table) #f))
    ))

;; ----------------------------------------

;; make-header-from-list : (Listof X) (X -> (values HeaderKey HeaderValueBytes))
;;                      -> header%
(define (make-header-from-list raw-header get-kv)
  (define table (make-hasheq))
  (define list-valued (make-hasheq))
  (for ([raw-field (in-list raw-header)])
    (define-values (key-bs val-bs) (get-kv raw-field))
    (define key (or (header-key->symbol key-bs #t)
                    (h-error "bad header field key\n  key: ~e" key-bs
                             #:info (hasheq 'who 'make-headers-from-list
                                            'code 'bad-header-field-key))))
    (cond [(hash-ref table key #f)
           => (lambda (old-v)
                (define old-v* (if (bytes? old-v) (list old-v) old-v))
                (define new-v (cons val-bs old-v*))
                (hash-set! table key new-v)
                (hash-set! list-valued key #t))]
          [else (hash-set! table key val-bs)]))
  (for ([k (in-hash-keys list-valued)])
    (hash-set! table k (reverse (hash-ref table k))))
  (new header% (table table)))

;; make-header-from-lines : (Listof Bytes) -> headers%
(define (make-header-from-lines raw-header)
  (make-header-from-list
   raw-header
   (lambda (line)
     (match (regexp-match (rx^$ HEADER-FIELD) line)
       [(list _ key-bs val-bs) (values key-bs val-bs)]
       [_ (h-error "malformed header field line\n  line: ~e" line
                   #:info (hasheq 'who 'make-header-from-lines
                                  'code 'bad-header-field-line))]))))


;; ============================================================
;; Checking FlexibleHeaderList

;; A HeaderEntry is (list Bytes Bytes) | (list Bytes Bytes 'never-add)

;; make-header-from-entries : (Listof HeaderEntry) -> header%
;; FIXME: preserve 'never-add ??
(define (make-header-from-entries entries)
  (make-header-from-list entries check-header-entry))

;; check-header-entry : Any -> (values HeaderKeyBytes HeaderValueBytes)
(define (check-header-entry entry)
  (match entry
    [(list* (? bytes? (and (or (regexp (rx^$ lower-TOKEN)) #":status") key))
            (? bytes? (and (regexp (rx^$ FIELD-VALUE)) value))
            (? (lambda (v) (member v '((never-add) ())))))
     (values key value)]
    [h (h-error "malformed header field entry\n  entry: ~e" h)]))

;; ============================================================
;; FlexibleHeaderList

;; A FlexibleHeaderList is (Listof FlexibleHeaderField).
;; A FlexibleHeaderField is one of
;; - (list FlexibleHeaderKey FlexibleHeaderValue)
;; - Bytes                  -- matching HEADER
;; - String                 -- matching HEADER
;; FlexibleHeaderKey = Symbol | Bytes | String  -- matching TOKEN
;; FlexibleHeaderValue = String | Bytes         -- matching OWS FIELD-VALUE OWS

;; check-flexible-header-list : FlexibleHeaderList -> (Listof HeaderEntry)
(define (check-flexible-header-list hs)
  (map check-flexible-header-field hs))
(define (check-flexible-header-field hf)
  (define (bad) (h-error "bad header field\n  field: ~e" hf))
  (match hf
    [(list key value)
     (list (check-flexible-header-key key) (check-flexible-header-value value))]
    [(? bytes?)
     (match (regexp-match (rx^$ HEADER-FIELD) hf)
       [(list _ key val)
        (list (check-flexible-header-key key) (bytes->immutable-bytes val))]
       [_ (bad)])]
    [(? string?)
     (match (regexp-match (rx^$ HEADER-FIELD) (string->bytes/utf-8 hf))
       [(list _ key val)
        (list (check-flexible-header-key key) (bytes->immutable-bytes val))]
       [_ (bad)])]
    [_ (bad)]))
(define (check-flexible-header-key key0)
  (define (imm bs) (bytes->immutable-bytes bs))
  (let loop ([key key0])
    (match key
      [(? symbol?) (loop (symbol->string key))]
      [(? bytes? (regexp (rx^$ TOKEN))) (imm key)]
      [(? string? (regexp (rx^$ TOKEN))) (imm (string->bytes/latin-1 key))]
      [else (h-error "bad header field key\n  key: ~e" key0)])))
(define (check-flexible-header-value value)
  (match value
    [(? bytes? (regexp (rx^$ (rx OWS (record FIELD-VALUE) OWS)) (list _ field-value)))
     (bytes->immutable-bytes field-value)]
    [(? string? (regexp (rx^$ (rx OWS (record FIELD-VALUE) OWS)) (list _ field-value)))
     (bytes->immutable-bytes (string->bytes/latin-1 field-value))]
    [_ (h-error "bad header field value\n  value: ~e" value)]))

(define (header-entries-missing? hs key)
  (not (assoc key hs)))


;; ============================================================
;; Header syntax utils

;; Reference: https://tools.ietf.org/html/rfc7230#appendix-B

(provide OWS TCHAR TOKEN)

(define-rx FIELD-VCHAR "[\x21-\x7E]")
(define-rx FIELD-CONTENT (rx FIELD-VCHAR (? (+ "[ \t]" FIELD-VCHAR))))
(define-rx FIELD-VALUE (* FIELD-CONTENT))

(define-rx HEADER-START (rx (record TOKEN) ":"))
(define-rx HEADER-FIELD (rx HEADER-START OWS (record FIELD-VALUE) OWS))

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
        [else (h-error "bad header field key\n  key: ~e" key
                       #:info (hasheq 'who 'header-key->symbol))]))

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
          [else (h-error "bad header field key\n  key: ~e" key0
                         #:info (hasheq 'who 'header-key->bytes))])))

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

(define reserved-header-keys
  '(host
    content-length
    connection
    keep-alive
    upgrade
    transfer-encoding
    te
    trailer))

(define reserved-header-keys/bytes
  '(#"host"
    #"content-length"
    #"connection"
    #"keep-alive"
    #"upgrade"
    #"transfer-encoding"
    #"te"
    #"trailer"))
