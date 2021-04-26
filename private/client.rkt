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

(define default-base-header
  '((#"accept-encoding" #"gzip, deflate")
    (#"user-agent" #"racket-http123/0.1")))

;; ============================================================

(define response/c (is-a?/c http-response<%>))
(define loc/c (or/c string? url?))
(define header/c (is-a?/c header<%>))
(define data/c (or/c #f bytes? (-> (-> bytes? any) any)))

(define http-client<%>
  (interface (http-client-base<%>)
    [sync-request
     (->m request? response/c)]
    ))

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
    ))

#;
(begin (define hs '((#"user-agent" #"Racket (http123)") (#"accept-encoding" #"gzip")))
       (define req (request 'GET "https://www.google.com/" hs #f))
       (define req1 (request 'GET "http://www.neverssl.com/" hs #f))
       (define c (new http-client%)))

;; FIXME: beware empty paths: (GET "https://www.google.com") fails!
