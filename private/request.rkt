;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require (for-syntax racket/base syntax/transformer)
         racket/match
         net/url-structs
         scramble/about
         "interfaces.rkt"
         "header-base.rkt"
         (submod "util.rkt" url))
(provide (except-out (all-defined-out) request)
         (rename-out [request* request]))

;; A Request is:
(struct request
  (method       ;; Symbol, like 'GET
   url          ;; URL
   header       ;; HeaderFieldList
   data         ;; (U #f Bytes (OutputPort -> Void))
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
                (check-header-field-list header)))
            (for ([hfield (in-list hs)])
              (match-define (list key val) hfield)
              (when (member key reserved-header-keys/bytes)
                (h-error "request contains header field reserved for user-agent\n  field: ~e" key
                         #:info (hasheq 'code 'reserved-request-header-field))))
            (unless (or (eq? #f data) (bytes? data) (procedure? data))
              (raise-argument-error 'request "(or/c #f bytes? procedure?)" data))
            (when (and data (memq 'forbid-request-body (hash-ref known-methods method)))
              (h-error "data forbidden for given method\n  method: ~e\n  data: ~e" method data
                       #:info (hasheq 'code 'request-method-forbids-data)))
            (values method u hs data))
  #:transparent
  #:property prop:about
  (lambda (self)
    (match-define (request method (url scheme _ host port _ _ _ _) _ _) self)
    (format "~a ~a://~a~a~a/..." method scheme host (if port ":" "") (or port ""))))

(define (make-request method url [header null] [data #f])
  (with-entry-point 'request
    (request method url header data)))

(define-match-expander request*
  (lambda (stx)
    (syntax-case stx ()
      [(_ method-p url-p header-p data-p)
       (syntax/loc stx (request method-p url-p header-p data-p))]))
  (make-variable-like-transformer #'make-request))

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

;; ----------------------------------------

(define (request-update req
                        #:add-header [inhfields null]
                        #:set-data [new-data (request-data req)])
  (match-define (request method url header data) req)
  (define hfields (check-header-field-list inhfields))
  (define new-header (header-field-list-update header hfields))
  (request method url new-header new-data))
