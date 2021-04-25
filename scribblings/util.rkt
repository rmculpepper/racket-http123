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
