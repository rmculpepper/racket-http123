;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

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
  (define (return key val)
    (when (member key reserved-header-keys/bytes)
      (h-error "request contains header field reserved for user-agent\n  field: ~e" key
               #:info (hasheq 'code 'reserved-request-header-field)))
    (list key val))
  (match hf
    [(list key value)
     (return (check-flexible-header-key key) (check-flexible-header-value value))]
    [(? bytes?)
     (match (regexp-match (rx^$ HEADER-FIELD) hf)
       [(list _ key val)
        (return (check-flexible-header-key key) (bytes->immutable-bytes val))]
       [_ (bad)])]
    [(? string?)
     (match (regexp-match (rx^$ HEADER-FIELD) (string->bytes/utf-8 hf))
       [(list _ key val)
        (return (check-flexible-header-key key) (bytes->immutable-bytes val))]
       [_ (bad)])]
    [_ (bad)]))
(define (check-flexible-header-key key0)
  (define (imm bs) (bytes->immutable-bytes bs))
  (let loop ([key key0])
    (match key
      [(? symbol?) (loop (symbol->string key))]
      [(? bytes? (regexp (rx^$ lower-TOKEN))) (imm key)]
      [(? string? (regexp (rx^$ lower-TOKEN))) (imm (string->bytes/latin-1 key))]
      [(? bytes? (regexp (rx^$ TOKEN))) (loop (bytes->string/latin-1 key))]
      [(? string? (regexp (rx^$ TOKEN))) (loop (string-downcase key))]
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

(define-rx qdtext #px#"[ \t\x21\x23-\\\x5B\\\x5D-\x7E]")
(define-rx quoted-pair #px#"\\\\[ \t\x21-\x7E]")
(define-rx quoted-string (rx "\"" (* (or qdtext quoted-pair)) "\""))


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
