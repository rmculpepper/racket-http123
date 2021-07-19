;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/class
         racket/contract/base
         racket/match
         scramble/class
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

(define (header-field-key? v)
  (and (bytes? v) (regexp-match-exact? (rx lower-TOKEN) v)))

(define header<%>
  (interface ()
    [get-header-field-list
     (->m (listof header-field/c))]
    [get-header-lines
     (->m (listof bytes?))]
    [has-key?
     (->m header-field-key? boolean?)]
    [get-values
     (->m header-field-key? (or/c (listof bytes?) #f))]
    [get-value
     (->m header-field-key? (or/c bytes? #f))]
    [get-ascii-string
     (->m header-field-key? (or/c string? #f))]
    [get-integer-value
     (->m header-field-key? (or/c exact-integer? #f))]
    [has-value?
     (->m header-field-key? bytes? boolean?)]
    [value-matches?
     (->*m [header-field-key? byte-regexp?] [#:exact? boolean?] boolean?)]
    [get-content-type
     (->m (or/c #f symbol?))]
    ))

;; ----------------------------------------

(define header%
  (class* object% (header<%> constructor-style-printable<%>)
    (init-field header-fields)  ;; HeaderFieldList
    (super-new)

    (field [table (make-hash)])   ;; Hasheq[Bytes => (U Bytes (Listof Bytes))]
    (let ([list-valued (make-hash)])
      (for ([hfield (in-list header-fields)])
        (match-define (list key-bs val-bs) hfield)
        (cond [(hash-ref table key-bs #f)
               => (lambda (old-v)
                    (define old-v* (if (bytes? old-v) (list old-v) old-v))
                    (define new-v (cons val-bs old-v*))
                    (hash-set! table key-bs new-v)
                    (hash-set! list-valued key-bs #t))]
              [else (hash-set! table key-bs val-bs)]))
      (for ([k (in-hash-keys list-valued)])
        (hash-set! table k (reverse (hash-ref table k)))))

    (define/public (get-header-field-list)
      header-fields)

    (define/public (get-header-lines)
      (for/list ([p (in-list (get-header-field-list))])
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
      (hash-remove! table key)
      (set! header-fields (filter (lambda (e) (not (equal? (car e) key))) header-fields)))

    ;; ----

    (define/public (get-content-type)
      (define content-type (or (get-value #"content-type") #""))
      (match (regexp-match (rx (rx ^ (record TOKEN) "/" (record TOKEN))) content-type)
        [(list _ type-bs subtype-bs)
         (string->symbol (format "~a/~a" type-bs subtype-bs))]
        [_ #f]))

    ;; ----

    (define/public (get-printing-class-name)
      'header%)
    (define/public (get-printing-components)
      (values '(header-fields) (list header-fields) #f))
    ))

;; ----------------------------------------

;; make-header-from-list : (Listof X) (X -> (list HeaderKey HeaderValueBytes))
;;                      -> header%
(define (make-header-from-list raw-header get-kv)
  (new header% (header-fields (map get-kv raw-header))))

;; make-header-from-lines : (Listof Bytes) -> headers%
(define (make-header-from-lines raw-header)
  (make-header-from-list
   raw-header
   (lambda (line)
     (match (regexp-match (rx^$ HEADER-FIELD) line)
       [(list _ key-bs val-bs)
        (list (check-header-field-key key-bs)
              (check-header-field-value val-bs))]
       [_ (h-error "malformed header field line\n  line: ~e" line
                   #:info (hasheq 'who 'make-header-from-lines
                                  'code 'bad-header-field-line))]))))

;; ============================================================
;; Checking FlexibleHeaderList

;; A HeaderEntry is (list Bytes Bytes) | (list Bytes Bytes 'never-add)

;; make-header-from-entries : (Listof HeaderEntry) -> header%
;; FIXME: preserve 'never-add ??
(define (make-header-from-entries entries)
  (make-header-from-list entries check-header-field))
