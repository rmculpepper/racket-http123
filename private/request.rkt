#lang racket/base
(require racket/match
         net/url-structs
         net/url-string
         "interfaces.rkt"
         "header.rkt"
         "util.rkt")
(provide (all-defined-out))

;; A Request is:
(struct request
  (method       ;; Symbol, like 'GET
   url          ;; URL
   header       ;; (Listof HeaderEntry)
   data         ;; (U #f Bytes ((Bytes -> Void) -> Void))
   )
  #:guard (lambda (method loc header data _)
            (define u
              (cond [(string? loc) (check-http-url 'request (string->url loc) loc)]
                    [(url? loc) (check-http-url 'request loc loc)]
                    [else (raise-argument-error 'request "(or/c string? url?)" loc)]))
            (define hs
              (with-entry-point 'request
                (check-flexible-header-list header)))
            (unless (or (eq? #f data) (bytes? data) (procedure? data))
              (raise-argument-error 'request "(or/c #f bytes? procedure?)" data))
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
