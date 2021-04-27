#lang racket/base
(require scribble/manual)
(provide (all-defined-out))

(define (h11rfc fragment . content)
  (apply hyperlink (format "https://tools.ietf.org/html/rfc7230#~a" fragment) content))

(define (rfc7231 fragment . content)
  (apply hyperlink (format "https://tools.ietf.org/html/rfc7231#~a" fragment) content))

(define (h2rfc fragment . content)
  (apply hyperlink (format "https://tools.ietf.org/html/rfc7540#~a" fragment) content))

(define (hpackrfc fragment . content)
  (apply hyperlink (format "https://tools.ietf.org/html/rfc7541#~a" fragment) content))


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
