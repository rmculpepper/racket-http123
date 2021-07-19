;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/class
         racket/contract/base
         racket/match
         net/url-structs
         "interfaces.rkt"
         "header.rkt"
         "request.rkt"
         "response.rkt"
         "connection.rkt"
         (submod "util.rkt" url))
(provide (all-defined-out))

;; ------------------------------------------------------------

(define http-client-base<%>
  (interface ()
    [async-request
     (->m request? (evt/c (-> (is-a?/c response<%>))))]
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
