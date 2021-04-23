;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/class
         racket/contract/base
         racket/match
         net/url-string
         "interfaces.rkt"
         "header.rkt"
         "request.rkt"
         "response.rkt"
         "connection.rkt"
         "util.rkt")
(provide (all-defined-out))

;; ============================================================

(define http-client-base<%>
  (interface ()
    [async-request
     (->m request? (evt/c (-> (is-a?/c http-response<%>))))]
    ))

;; ------------------------------------------------------------

;; Add helpers to
;; - create a client that uses a single connection
;;   (eg, for CONNECT tunneling)
;; - specialize a client (shared connection table) to add different default header, etc
;; - add url-for-connection hook, takes request loc, produces url to connect to

;; generally, need to figure out requirements for proxies

(define connection-manager%
  (class* object% ()
    (init-field [ssl 'secure])
    (super-new)

    ;; connections : Hash[(list String Nat Boolean) => Connection]
    ;; Currently, limit to one connection per host/port/ssl.
    (define connections (make-hash))

    (define/public (get-connection loc)
      (define u (check-http-url 'get-connection loc))
      (get-connection* (url-host u)
                       (or (url-port u) (scheme-default-port (url-scheme u)))
                       (case (url-scheme u) [("https") #t] [else #f])))

    (define/public (get-connection* host port ssl?)
      (define key (list host port ssl?))
      (hash-ref! connections key (lambda () (open-connection host port (and ssl? ssl)))))

    (define/public (open-connection host port ssl)
      (log-http-debug "opening connection: ~.s:~.s (ssl=~e)" host port ssl)
      (new http-connection% (host host) (port port) (ssl ssl)))

    (define/public (async-request req)
      (send (get-connection (request-url req)) async-request req))
    ))


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

;; ------------------------------------------------------------

(define http-client%
  (class* object% (http-client<%>)
    (init-field [base (new connection-manager%)])
    (super-new)

    (define/public (async-request req)
      (send base async-request req))

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
       (define req (request 'GET (string->url "https://www.google.com/") hs #f))
       (define req1 (request 'GET (string->url "http://www.neverssl.com/") hs #f))
       (define c (new http-client%)))

;; FIXME: beware empty paths: (GET "https://www.google.com") fails!
