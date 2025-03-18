;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/match
         racket/contract/base
         racket/list
         racket/symbol
         "interfaces.rkt"
         scramble/regexp
         (submod "util.rkt" regexp))
(provide (all-defined-out))

;; References:
;; - HTTP/1.1: https://tools.ietf.org/html/rfc7230


;; ============================================================
;; Header syntax utils

;; Reference: https://tools.ietf.org/html/rfc7230#appendix-B

(provide OWS TCHAR TOKEN)

(define-RE FIELD-VCHAR #:byte (chars [#x21 #x7E]))
;;(define-RE FIELD-CONTENT #:byte (cat FIELD-VCHAR (? (cat (+ (chars " \t")) FIELD-VCHAR))))
;;(define-RE FIELD-VALUE #:byte (* FIELD-CONTENT))
(define-RE FIELD-VALUE #:byte (or "" (cat FIELD-VCHAR (* (cat (* (chars " \t")) FIELD-VCHAR)))))

(define-RE HEADER-START #:byte (cat (report TOKEN) ":"))
(define-RE HEADER-FIELD #:byte (cat HEADER-START OWS (report FIELD-VALUE) OWS))

(define-RE TOKEN+ #:byte (cat TOKEN (* (cat OWS "," OWS TOKEN))))

(define-RE qdtext #:byte (chars #\space #\tab #x21 [#x23 #x5B] [#x5D #x7E]))
(define-RE quoted-pair #:byte (cat "\\" (chars #\space #\tab [#x21 #x7E])))
(define-RE quoted-string #:byte (cat "\"" (* (or qdtext quoted-pair)) "\""))


;; ============================================================
;; HeaderFieldList

;; InHeaderFieldList = (Listof InHeaderField)
;; InHeaderField is one of
;; - (list InHeaderFieldKey InHeaderFieldValue)
;; - Bytes                                      -- matching HEADER-FIELD
;; - String                                     -- matching HEADER-FIELD
;; InHeaderFieldKey = Symbol | Bytes | String   -- matching TOKEN
;; InHeaderFieldValue = String | Bytes          -- matching OWS FIELD-VALUE OWS

(define in-header-field/c
  (or/c string? bytes? (list/c (or/c symbol? string? symbol?) (or/c string? bytes?))))

;; HeaderFieldList = (Listof NormalHeaderField)
;; HeaderField = (list HeaderFieldKey HeaderFieldValue)
;; HeaderFieldKey = ImmutableBytes              -- matching lower-TOKEN
;; HeaderFieldValue = ImmutableBytes            -- matching FIELD-VALUE

(define (header-field-key/c v)
  (and (bytes? v) (immutable? v) (regexp-match-exact? lower-TOKEN v)))
(define (header-field-value/c v)
  (and (bytes? v) (immutable? v) (regexp-match-exact? FIELD-VALUE v)))
(define header-field/c (list/c header-field-key/c header-field-value/c))

;; check-header-field-list : InHeaderFieldList -> HeaderFieldList
(define (check-header-field-list hs)
  (map check-header-field hs))

;; check-header-field : InHeaderField -> HeaderField
(define (check-header-field hf)
  (define (bad) (h-error "malformed header field\n  field: ~e" hf
                         #:info (hasheq 'code 'malformed-header-field)))
  (match hf
    [(list key value)
     (list (check-header-field-key key) (check-header-field-value value))]
    [(? bytes?)
     (match (regexp-match (byte-px^$ HEADER-FIELD) hf)
       [(list _ key val)
        (list (check-header-field-key key) (bytes->immutable-bytes val))]
       [_ (bad)])]
    [(? string?)
     (match (regexp-match (byte-px^$ HEADER-FIELD) (string->bytes/utf-8 hf))
       [(list _ key val)
        (list (check-header-field-key key) (bytes->immutable-bytes val))]
       [_ (bad)])]
    [_ (bad)]))

;; check-header-field-key : InHeaderFieldKey -> HeaderFieldKey
(define (check-header-field-key key0)
  (define (imm bs)
    (or (hash-ref-key common-bytes=>symbol bs #f)
        (bytes->immutable-bytes bs)))
  (let loop ([key key0])
    (match key
      [(? symbol?) (loop (symbol->string key))]
      [(? bytes? (regexp (byte-px^$ lower-TOKEN))) (imm key)]
      [(? string? (regexp (byte-px^$ lower-TOKEN))) (imm (string->bytes/latin-1 key))]
      [(? bytes? (regexp (byte-px^$ TOKEN))) (loop (bytes->string/latin-1 key))]
      [(? string? (regexp (byte-px^$ TOKEN))) (loop (string-downcase key))]
      [else (h-error "bad header field key\n  key: ~e" key0
                     #:info (hasheq 'code 'bad-header-field-key))])))

;; check-header-field-value : InHeaderFieldValue -> HeaderFieldValue
(define (check-header-field-value value)
  (match value
    [(? bytes? (regexp (byte-px^$ OWS (report FIELD-VALUE) OWS) (list _ field-value)))
     (bytes->immutable-bytes field-value)]
    [(? string? (regexp (byte-px^$ OWS (report FIELD-VALUE) OWS) (list _ field-value)))
     (bytes->immutable-bytes field-value)]
    [_ (h-error "bad header field value\n  value: ~e" value
                #:info (hasheq 'code 'bad-header-field-value))]))

;; header-field-list-missing? : HeaderFieldList Bytes -> Boolean
(define (header-field-list-missing? hs key)
  (not (assoc key hs)))

;; header-field->line : HeaderField -> Bytes
(define (header-field->line hfield)
  (match-define (list key val) hfield)
  (bytes-append key #": " val))

;; header-field-list-update : HeaderFieldList HeaderFieldList -> HeaderFieldList
;; Remove all fields of hfs with keys present in new-hfs, then append new-hfs.
(define (header-field-list-update hfs new-hfs)
  (if (null? new-hfs)
      hfs
      (append (filter (lambda (hf) (not (assoc (car hf) new-hfs))) hfs)
              new-hfs)))


;; ============================================================
;; Header key forms

(define (header-key-symbol? v)
  (and (symbol? v)
       (or (and (common-symbol->bytes v) #t)
           (header-key-name? (symbol->immutable-string v)))))

#;
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
  (regexp-match? (byte-px^$ lower-TOKEN) s))

(define (header-key-name-ci? s)
  (regexp-match (byte-px^$ TOKEN) s))

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
;; Reserved header field keys

(define reserved-header-keys/bytes
  '(#"host"
    #"content-length"
    #"connection"
    #"keep-alive"
    #"upgrade"
    #"transfer-encoding"
    #"te"
    #"trailer"))


;; ============================================================
;; Misc Bytes Utils

(define (bytes->nat v) ;; Bytes -> Nat or #f
  (define s (and v (bytes->string/latin-1 v)))
  (define n (and s (string->number s)))
  (and (exact-nonnegative-integer? n) n))

(define (bytes-join bss sep)
  (apply bytes-append (add-between bss sep)))

(define (symbol->bytes s)
  (string->bytes/utf-8 (symbol->string s)))
