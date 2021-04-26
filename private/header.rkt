;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/class
         racket/contract/base
         racket/match
         "interfaces.rkt"
         "regexp.rkt"
         "header-base.rkt"
         "util.rkt")
(provide (all-defined-out)
         (all-from-out "header-base.rkt"))

;; References:
;; - HTTP/1.1: https://tools.ietf.org/html/rfc7230

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
    [get-table
     (->m (hash/c header-key-symbol? (or/c bytes? (listof bytes?))))]
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
    [get-content-type
     (->m (or/c #f symbol?))]
    ))

;; ----------------------------------------

(define header%
  (class* object% (header<%> class-printable<%>)
    (init-field [table (make-hasheq)]) ;; Hasheq[Symbol => (U Bytes (Listof Bytes))]
    (super-new)

    (define/public (get-table) table)
    (define/public (copy-table) (hash-copy table))

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

    (define/public (get-content-type)
      (define content-type (or (get-value 'content-type) #""))
      (match (regexp-match (rx (rx ^ (record TOKEN) "/" (record TOKEN))) content-type)
        [(list _ type-bs subtype-bs)
         (string->symbol (format "~a/~a" type-bs subtype-bs))]
        [_ #f]))

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
