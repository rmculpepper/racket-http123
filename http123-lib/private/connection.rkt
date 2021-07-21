;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/class
         racket/contract/base
         racket/match
         racket/tcp
         net/url-structs
         openssl
         "interfaces.rkt"
         "request.rkt"
         "header.rkt"
         "response.rkt"
         "http11.rkt"
         "http2.rkt"
         (submod "util.rkt" url))
(provide (all-defined-out))

;; ============================================================
;; Connection

(define http-connection<%>
  (interface ()
    [async-request
     (->m request? (evt/c (-> any)))]
    ))

(define have-alpn?
  (let-values ([(req-kws opt-kws) (procedure-keywords ssl-connect)])
    (and (memq '#:alpn (or opt-kws null)) #t)))
(unless have-alpn?
  (log-http-warning "http/2 not available because ssl-connect does not support ALPN"))

(define http-connection%
  (class* object% (#; http-connection<%>)
    (init-field host
                port
                ssl
                [protocols '(http/2 http/1.1)]
                [custodian (current-custodian)])
    (super-new)

    (define/public (get-host) host)
    (define/public (get-port) port)

    (define lock (make-semaphore 1))
    (define-syntax-rule (with-lock e ...) ;; doesn't unlock on escape
      (begin (semaphore-wait lock) (begin0 (let () e ...) (semaphore-post lock))))

    (define conn #f)

    (define/public (get-actual-connection [connect? #t])
      (with-lock
        (cond [(and conn (send conn open?))
               (log-http-debug "using existing connection")
               conn]
              [connect?
               (let ([c (with-handlers ([exn? (lambda (e)
                                                (semaphore-post lock)
                                                (raise e))])
                          (parameterize ((current-custodian custodian))
                            (open-actual-connection)))])
                 (log-http-debug "created new actual connection")
                 (begin (set! conn c) c))]
              [else #f])))

    (define/private (open-actual-connection)
      (define try-http1? (memq 'http/1.1 protocols))
      (define try-http2? (and (memq 'http/2 protocols) have-alpn?))
      (log-http-debug "connecting to ~e" (format "~a:~a" host port))
      (cond [(not ssl)
             (define-values (in out)
               (tcp-connect host port))
             (log-http-debug "connected without TLS, http/1.1")
             (make-http1 in out)]
            [(and try-http1? try-http2?)
             (define-values (in out)
               (ssl-connect host port ssl #:alpn '(#"h2" #"http/1.1")))
             (case (ssl-get-alpn-selected in)
               [(#"h2")
                (log-http-debug "connected with TLS, ALPN=h2")
                (make-http2 in out)]
               [(#"http/1.1")
                (log-http-debug "connected with TLS, ALPN=http/1.1")
                (make-http1 in out)]
               [else
                (log-http-debug "connected with TLS, http/1.1 (no ALPN selected)")
                (make-http1 in out)])]
            [try-http2?
             (define-values (in out)
               (ssl-connect host port ssl #:alpn '(#"h2")))
             (case (ssl-get-alpn-selected in)
               [(#"h2")
                (log-http-debug "connected with TLS, ALPN=h2")
                (make-http2 in out)]
               [else
                (log-http-debug "connected with TLS, no ALPN selected, failing")
                (close-input-port in)
                (close-output-port out)
                (h-error "connection error: http/2 not supported~a"
                         ";\n the server accepted the connection but did not select ALPN=h2")])]
            [try-http1?
             (define-values (in out)
               (ssl-connect host port ssl))
             (log-http-debug "connected with TLS, http/1.1 (did not use ALPN)")
             (make-http1 in out)]
            [else (h-error "no protocols available")]))

    (define/private (make-http1 in out)
      (new http11-actual-connection% (parent this)
           (in in) (out out)))
    (define/private (make-http2 in out)
      (new http2-actual-connection% (parent this)
           (in in) (out out)))

    (define/public (close)
      (with-lock
        (when conn
          (define c conn)
          (send c abandon)
          (set! conn #f))))

    ;; ----------------------------------------

    (define/public (on-actual-disconnect ac)
      (with-lock
        (when (eq? conn ac)
          (log-http-debug "disconnected actual connection")
          (set! conn #f))))

    ;; ----------------------------------------

    ;; async-request : Request -> (BoxEvt (-> Response))
    (define/public (async-request req)
      (define TRIES 2)
      (let loop ([attempts 0])
        (unless (< attempts TRIES)
          (h-error "failed to send request (after ~s attempts)" attempts))
        (define ac (get-actual-connection))
        (cond [(send ac open-request req) => values]
              [else (begin (send ac abandon) (loop (add1 attempts)))])))
    ))


;; ============================================================
;; Connection manager

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
    (init-field [ssl 'secure]
                [custodian (current-custodian)])
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
      (new http-connection% (host host) (port port) (ssl ssl) (custodian custodian)))

    (define/public (async-request req)
      (send (get-connection (request-url req)) async-request req))
    ))
