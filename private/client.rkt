;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/class
         racket/contract/base
         racket/match
         net/url-structs
         "interfaces.rkt"
         "header-base.rkt"
         "header.rkt"
         "request.rkt"
         "response.rkt"
         "connection.rkt"
         "client-base.rkt"
         "util.rkt")
(provide (all-defined-out))

;; ============================================================

(define response/c (is-a?/c http-response<%>))
(define loc/c (or/c string? url?))
(define header/c (is-a?/c header<%>))
(define data/c (or/c #f bytes? (-> (-> bytes? any) any)))

(define data-method/c
  (->*m [loc/c]
        [#:header header/c
         #:data data/c]
        any))
(define nodata-method/c
  (->*m [loc/c]
        [#:header header/c]
        any))

(define http-client<%>
  (interface (http-client-base<%>)
    [sync-request
     (->m request? response/c)]
    [GET data-method/c]
    [HEAD nodata-method/c]
    [POST data-method/c]
    [PUT data-method/c]
    [PATCH data-method/c]
    [DELETE nodata-method/c]
    [OPTIONS nodata-method/c]
    [TRACE nodata-method/c]
    ))

(define default-base-header
  '((#"accept-encoding" #"gzip, deflate")
    (#"user-agent" #"racket-http123/0.1")))

;; ============================================================

(define http-client%
  (class* object% (http-client<%>)
    (init-field [base (new connection-manager%)]
                [base-header default-base-header])
    (super-new)

    (define/public (fork #:base-header [base-header base-header]
                         #:add-header [add-header null])
      (new http-client% (base base)
           (base-header (header-field-list-update base-header add-header))))

    (define/public (adjust-request req)
      (match-define (request method url header data) req)
      (define new-header (header-field-list-update base-header header))
      (request method url new-header data))

    ;; ----------------------------------------

    (define/public (async-request req)
      (send base async-request (adjust-request req)))

    (define/public (sync-request req)
      ((sync (async-request req))))

    ;; ----------------------------------------
    ;; Reference: https://tools.ietf.org/html/rfc7231

    (define-syntax-rule (make-method method)
      (lambda (loc #:header [header null] #:data [data #f])
        (with-entry-point 'method
          (sync-request (request 'method loc header data)))))
    (define-syntax-rule (make-method/no-data method)
      (lambda (loc #:header [header null])
        (with-entry-point 'method
          (sync-request (request 'method loc header #f)))))

    (define/public GET (make-method 'GET))
    (define/public HEAD (make-method/no-data 'HEAD))
    (define/public POST (make-method 'POST))
    (define/public PUT (make-method 'PUT))
    (define/public PATCH (make-method 'PATCH))
    (define/public DELETE (make-method/no-data 'DELETE))
    (define/public OPTIONS (make-method/no-data 'OPTIONS))
    (define/public TRACE (make-method/no-data 'TRACE))
    ))

#;
(begin (define hs '((#"user-agent" #"Racket (http123)") (#"accept-encoding" #"gzip")))
       (define req (request 'GET "https://www.google.com/" hs #f))
       (define req1 (request 'GET "http://www.neverssl.com/" hs #f))
       (define c (new http-client%)))

;; FIXME: beware empty paths: (GET "https://www.google.com") fails!
