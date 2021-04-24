;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

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
            (unless (memq method known-method-names)
              (define parts
                (apply string-append (map (lambda (n) (format " '~a" n)) known-method-names)))
              (raise-argument-error 'request (format "(or/c~a)" parts) method))
            (define u
              (cond [(or (url? loc) (string? loc)) (check-http-url 'request loc loc)]
                    [else (raise-argument-error 'request "(or/c string? url?)" loc)]))
            (define hs
              (with-entry-point 'request
                (check-flexible-header-list header)))
            (unless (or (eq? #f data) (bytes? data) (procedure? data))
              (raise-argument-error 'request "(or/c #f bytes? procedure?)" data))
            (values method u hs data))
  #:transparent
  #:property prop:about
  (lambda (self)
    (match-define (request method (url scheme _ host port _ _ _ _) _ _) self)
    (format "~a ~a://~a~a~a/..." method scheme host (if port ":" "") (or port ""))))

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

;; flags : safe, idempotent, {undef,forbid}-request-body, never-response-body
;; - 'undef-request-body - standard assigns no semantics, but not forbidden
;; - 'forbid-request-body - client MUST NOT send body
;; - 'never-response-body - no response body, regardless of status
(define known-methods
  #hasheq((GET     . (safe undef-request-body))
          (HEAD    . (safe undef-request-body never-response-body))
          (POST    . ())
          (PUT     . (idmp))
          (DELETE  . (idmp undef-request-body))
          (OPTIONS . (safe))
          (TRACE   . (safe forbid-request-body))
          (CONNECT . (disallow undef-request-body))

          (PATCH   . ()) ;; RFC 5789
          ))

(define known-method-names
  '(GET HEAD POST PUT DELETE OPTIONS TRACE PATCH))
