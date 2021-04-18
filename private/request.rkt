#lang racket/base
(require racket/match
         net/url-structs
         net/url-string
         "header.rkt"
         "util.rkt")
(provide (all-defined-out))

;; A Request is:
(struct request
  (method       ;; Symbol, like 'GET
   url          ;; URL
   header       ;; (Listof HeaderEntry)
   data         ;; (U #f Bytes (Bytes -> Void))
   )
  #:guard (lambda (method loc header data _)
            (define (bad argn expected)
              (raise-argument-error 'request expected argn method loc header))
            (define u
              (cond [(string? loc) (check-http-url 'request (string->url loc) loc)]
                    [(url? loc) (check-http-url 'request loc loc)]
                    [else (bad 1 "(or/c string? url?)")]))
            (define hs (check-flexible-header-list header))
            (values method u hs data))
  #:transparent)

;; request:can-replay? : Request -> Boolean
;; Can the request be replayed in a different actual connection?  Only care
;; about request semantics, not effect on connection state (eg close, upgrade).
(define (request:can-replay? req)
  ;; FIXME: depends on policy, belongs in connection?
  (match req
    [(request method url header data)
     (and (method:idempotent? method)
          (or (eq? data #f) (bytes? data)))]))

;; ----------------------------------------

;; RFC 7231 4.2.1 (Safe Methods)
(define (method:safe? method)
  (memq method '(GET HEAD OPTIONS TRACE)))

;; RFC 7231 4.2.? (Idempotent Methods)
(define (method:idempotent? method)
  (or (method:safe? method) (memq method '(PUT DELETE))))
