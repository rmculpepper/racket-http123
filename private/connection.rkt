#lang racket/base
(require racket/class
         racket/match
         racket/tcp
         net/url-string
         openssl
         "interfaces.rkt"
         "request.rkt"
         "http11.rkt"
         "http2.rkt")
(provide (all-defined-out))

(define (connect host port ssl)
  (new http-connection% (host host) (port port) (ssl ssl)))


;; FIXME: need way of restricting versions to allow eg for tunneling
;; (only http/1.1), eg websockets
;; - NO, makes more sense for tunneling to require creating a fresh
;;   http/1.1 connection specifically for the tunnel
;; - but restricting version is good for testing

(define http-connection%
  (class* object% (#; http-connection<%>)
    (init-field host
                port
                ssl)
    (super-new)

    (define/public (get-host) host)
    (define/public (get-port) port)

    (define lock (make-semaphore 1))
    (define-syntax-rule (with-lock e ...) ;; doesn't unlock on escape
      (begin (semaphore-wait lock) (begin0 (let () e ...) (semaphore-post lock))))

    ;; FIXME: need marker for connections that always error (eg, not an HTTP server)
    (define conn #f)

    (define/public (get-actual-connection [connect? #t])
      (with-lock
        (cond [(and conn (send conn live?))
               (log-http-debug "using existing connection")
               conn]
              [connect?
               (let ([c (with-handlers ([exn? (lambda (e)
                                                (semaphore-post lock)
                                                (raise e))])
                          (open-actual-connection))])
                 (log-http-debug "created new actual connection")
                 (begin (set! conn c) c))]
              [else #f])))

    (define/private (open-actual-connection)
      (cond [ssl
             (define-values (in out)
               (ssl-connect host port ssl #:alpn '(#"h2" #"http/1.1")))
             (case (ssl-get-alpn-selected in)
               [(#"h2")
                (log-http-debug "connected with TLS, ALPN=h2")
                (make-http2 in out)]
               [(#"http/1.1")
                (log-http-debug "connected with TLS, ALPN=http/1.1")
                (make-http1 in out #f)]
               [else
                (log-http-debug "connected with TLS, no ALPN, so http/1.1")
                (make-http1 in out #f)])]
            [else
             (define-values (in out)
               (tcp-connect host port))
             (log-http-debug "connected without TLS, http/1.1")
             (make-http1 in out #t)]))

    (define/private (make-http1 in out try-upgrade?)
      (new http11-actual-connection% (parent this)
           (in in) (out out) (try-upgrade? try-upgrade?)))
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

    (define/public (url->host-bytes u)
      (define scheme (url-scheme u))
      (define host (or (url-host u) (get-host)))
      (define port (or (url-port u) (get-port)))
      (string->bytes/utf-8
       (cond [(= port (case scheme [("http") 80] [("https") 443] [else #f])) host]
             [else (format "~a:~a" host port)])))

    ;; ----------------------------------------

    ;; sync-request : Request -> Response
    (define/public (sync-request req)
      (sync (async-request req)))

    ;; async-request : Request -> Evt[Response]
    (define/public (async-request req)
      (define TRIES 2)
      (let loop ([attempts 0])
        (unless (< attempts TRIES)
          (h-error "failed to send request (after ~s attempts)" attempts))
        (define ac (get-actual-connection))
        (cond [(send ac open-request req) => values]
              [else (begin (send ac abandon) (loop (add1 attempts)))])))

    ))

#;
(begin (define hs '((#"user-agent" #"Racket (http123)") (#"accept-encoding" #"gzip")))
       (define req (request 'GET (string->url "https://www.google.com/") hs #f))
       (define c (connect "www.google.com" 443 'auto)))
#; (define r (send c sync-request req))

#;
(begin (define req1 (request 'GET (string->url "http://www.neverssl.com/") hs #f))
       (define c1 (connect "www.neverssl.com" 80 #f)))
#; (define r1 (send c1 sync-request req1))
