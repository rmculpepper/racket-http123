#lang racket/base
(require racket/class
         racket/match
         racket/list
         racket/port
         racket/tcp
         net/url-structs
         net/url-string
         binaryio/reader
         openssl
         "interfaces.rkt"
         "header.rkt"
         "regexp.rkt"
         "io.rkt"
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
               conn]
              [connect?
               (let ([c (with-handlers ([exn? (lambda (e)
                                                (semaphore-post lock)
                                                (raise e))])
                          (open-actual-connection))])
                 (begin (set! conn c) c))]
              [else #f])))

    (define/private (open-actual-connection)
      (cond [ssl
             (define-values (in out)
               (ssl-connect host port ssl #:alpn '(#"h2" #"http/1.1")))
             (cond [(equal? #"h2" (ssl-get-alpn-selected in))
                    (make-http2 in out)]
                   [else (make-http1 in out #f)])]
            [else
             (define-values (in out)
               (tcp-connect host port))
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

    (define/public (url->host-bytes u)
      (define scheme (url-scheme u))
      (define host (or (url-host u) (get-host)))
      (define port (or (url-port u) (get-port)))
      (string->bytes/utf-8
       (cond [(= port (case scheme [("http") 80] [("https") 443] [else #f])) host]
             [else (format "~a:~a" host port)])))

    ;; ----------------------------------------

    ;; sync-request : Request CControl -> Response
    (define/public (sync-request req ccontrol)
      (sync (async-request req ccontrol)))

    ;; async-request : Request CControl -> Evt[Response]
    (define/public (async-request req ccontrol)
      (define TRIES 2)
      (let loop ([attempts 0])
        (unless (< attempts TRIES)
          (error* "failed to send request (after ~s attempts)" attempts))
        (define ac (get-actual-connection))
        (cond [(send ac open-request req ccontrol) => values]
              [else (begin (send ac abandon) (loop (add1 attempts)))])))

    ))

#;
(begin (define hs '((#"user-agent" #"Racket (http123)") (#"accept-encoding" #"gzip")))
       (define req (request 'GET (string->url "https://www.google.com/") hs #f))
       (define c (connect "www.google.com" 443 'auto))
       (define r (send c sync-request req #f)))
