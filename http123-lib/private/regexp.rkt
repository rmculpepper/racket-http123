;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require (for-syntax racket/base racket/list racket/syntax syntax/parse))
(provide (all-defined-out))

(begin-for-syntax
  (struct rx (src))

  (define (concat . parts)
    (define (convert p) (if (string? p) (string->bytes/utf-8 p) p))
    (apply bytes-append (map convert parts)))
  (define (w part)
    (bytes-append #"(?:" part #")"))
  (define (alternate parts)
    (apply bytes-append (add-between parts #"|")))

  (define ((bad-regexp-error stx re-stx) str)
    (raise-syntax-error #f str stx re-stx))

  (define-syntax-class bytes-pregexp #:attributes () #:opaque
    (pattern d #:when (byte-pregexp? (syntax-e #'d))))

  (define-syntax-class RE
    #:attributes (code) ; Expr1[String]
    #:datum-literals (^ $ quote * + ? or rx record)
    (pattern ^
             #:with code #'(quote #"^"))
    (pattern $
             #:with code #'(quote #"$"))
    (pattern (~var rxname (static rx? "name defined as regexp"))
             #:with code #`(quote #,(rx-src (attribute rxname.value))))
    (pattern (quote s:string)
             #:with code (let ([bs (string->bytes/utf-8 (syntax->datum #'s))])
                           #`(quote #,(regexp-quote bs))))
    (pattern s:string
             #:with code #`(quote #,(string->bytes/utf-8 (syntax->datum #'s))))
    (pattern r:bytes-pregexp
             #:with code #`(quote #,(object-name (syntax->datum #'r))))
    (pattern (* re:RE ...+)
             #:with code #`(concat "(?:" (w re.code) ... ")*"))
    (pattern (+ re:RE ...+)
             #:with code #`(concat "(?:" (w re.code) ... ")+"))
    (pattern (? re:RE ...+)
             #:with code #`(concat "(?:" (w re.code) ... ")?"))
    (pattern (or re:RE ...+)
             #:with code #`(alternate (list (w re.code) ...)))
    (pattern (rx (~optional (~seq #:prefix prefix:string)) re:RE ...)
             #:with code #`(concat "(" (~? prefix "?:") (w re.code) ... ")"))
    (pattern (record re:RE ...)
             #:with code #`(concat "(" (w re.code) ... ")"))))

(define-syntax define-rx
  (syntax-parser
    [(_ name:id re:RE)
     #`(begin (define-syntax name (rx re.code)))]))

(define-syntaxes (rx rx^ rx^$)
  (let ()
    (define (tx pre post)
      (syntax-parser
        [(_ re:RE)
         (define re-string
           (bytes-append pre (syntax-local-eval #'re.code) post))
         (define re-lit (byte-pregexp re-string (bad-regexp-error this-syntax #'re)))
         #`(quote #,re-lit)]))
    (values (tx #"" #"") (tx #"^" #"") (tx #"^" #"$"))))

(define-rx OWS "[ \t]*")
(define-rx TCHAR "[-^!#$&'*+._`|~a-zA-Z0-9]")
(define-rx TOKEN (+ TCHAR))

(define-rx lower-TCHAR "[-^!#$&'*+._`|~a-z0-9]")
(define-rx lower-TOKEN (+ lower-TCHAR))
