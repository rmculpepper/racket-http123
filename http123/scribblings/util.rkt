#lang racket/base
(require (for-syntax racket/base)
         (only-in scribble/racket make-element-id-transformer)
         scribble/core scribble/html-properties scribble/manual)
(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(define (h11rfc fragment . content)
  (apply hyperlink (format "https://tools.ietf.org/html/rfc7230#~a" fragment) content))

(define (rfc7231 fragment . content)
  (apply hyperlink (format "https://tools.ietf.org/html/rfc7231#~a" fragment) content))

(define (h2rfc fragment . content)
  (apply hyperlink (format "https://tools.ietf.org/html/rfc7540#~a" fragment) content))

(define (hpackrfc fragment . content)
  (apply hyperlink (format "https://tools.ietf.org/html/rfc7541#~a" fragment) content))

(define small-caps-style
  (style #f (list (attributes '((style . "font-variant: small-caps"))))))

(define (HTTP/1.1) "HTTP/1.1" #;(elem #:style small-caps-style "http/1.1"))
(define (HTTP/2) "HTTP/2" #;(elem #:style small-caps-style "http/2"))
(define (HTTP) "HTTP" #;(elem #:style small-caps-style "http"))

(begin-for-syntax
  (define (method-name-tx class/ifc-id method-id)
    (make-element-id-transformer
     (lambda (stx) #`(method #,class/ifc-id #,method-id)))))

;; ----------------------------------------

(module pretty racket/base
  (require racket/serialize racket/port racket/pretty)
  (serializable-struct pretty (s)
    #:property prop:custom-write
    (lambda (self out mode) (write-string (pretty-s self) out)))
  (define-syntax-rule (P expr)
    (pretty (call-with-output-string
             (lambda (out)
               (parameterize ((pretty-print-columns 80))
                 (pretty-print expr out))))))
  (provide (all-defined-out)))
