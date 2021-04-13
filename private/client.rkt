#lang racket/base
(require racket/class
         racket/match
         net/url-string
         "interfaces.rkt"
         "request.rkt"
         "connection.rkt")
(provide (all-defined-out))

(define DEFAULT-SCHEME "https")

(define http-client%
  (class* object% ()
    (init-field [ssl 'secure])
    (super-new)

    ;; connections : Hash[(list String Nat Boolean) => Connection]
    ;; Currently, limit to one connection per host/port/ssl.
    (define connections (make-hash))

    (define/public (get-connection* host port ssl?)
      (define key (list host port (and ssl? #t)))
      (hash-ref! connections key
                 (lambda ()
                   (log-http-debug "opening connection: ~.s" key)
                   (new http-connection% (host host) (port port) (ssl (and ssl? ssl))))))

    (define/public (get-connection u)
      (define scheme (url-scheme u))
      (unless scheme
        (h-error "missing scheme in URL\n  url: ~e" u))
      (unless (member scheme '("http" "https"))
        (h-error "unsupported scheme in URL\n  url: ~e" u))
      (define host (url-host u))
      (define port (or (url-port u) (default-port-for-scheme scheme)))
      (unless host
        (h-error "missing host in URL\n  url: ~e" u))
      (get-connection* host port (case scheme [("https") #t] [else #f])))

    (define/public (sync-request req)
      (sync (async-request req)))
    (define/public (async-request req)
      (send (get-connection (request-url req)) async-request req))

    ;; ------------------------------------------------------------

    (define/public (GET loc #:headers [headers null] #:data [data #f])
      (define req (request 'GET (->url loc) headers data))
      (sync-request req))
    (define/public (POST loc #:headers [headers null] #:data [data #f])
      (define req (request 'POST (->url loc) headers data))
      (sync-request req))
    ))

(define (default-port-for-scheme scheme)
  (case scheme [("http") 80] [("https") 443]))

(define (->url loc) (if (string? loc) (string->url loc) loc))

#;
(begin (define hs '((#"user-agent" #"Racket (http123)") (#"accept-encoding" #"gzip")))
       (define req (request 'GET (string->url "https://www.google.com/") hs #f))
       (define req1 (request 'GET (string->url "http://www.neverssl.com/") hs #f))
       (define c (new http-client%)))

;; FIXME: beware empty paths: (GET "https://www.google.com") fails!
