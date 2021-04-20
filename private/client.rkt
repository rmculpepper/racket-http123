#lang racket/base
(require racket/class
         racket/contract/base
         racket/match
         net/url-string
         "interfaces.rkt"
         "request.rkt"
         "response.rkt"
         "connection.rkt"
         "util.rkt")
(provide (all-defined-out))

(define http-client-base<%>
  (interface ()
    [async-request (-> request? (evt/c (-> (is-a?/c http-response<%>))))]
    ))

(define http-client<%>
  (interface (http-client-base<%>)
    ))

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

    (define/public (get-connection* host port ssl?)
      (define key (list host port (and ssl? #t)))
      (hash-ref! connections key
                 (lambda ()
                   (log-http-debug "opening connection: ~.s" key)
                   (new http-connection% (host host) (port port) (ssl (and ssl? ssl))))))

    (define/public (get-connection loc)
      (define u (->url loc))
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

    (define/public (async-request req)
      (send (get-connection (request-url req)) async-request req))
    ))

(define (client-mixin %)
  (class* % ()
    (inherit async-request)
    (super-new)

    (define/public (sync-request req)
      ((sync (async-request req))))

    ;; Reference: https://tools.ietf.org/html/rfc7231

    (define-syntax-rule (make-method method)
      (lambda (loc #:header [header null] #:data [data #f])
        (do-method 'method loc header data)))
    (define-syntax-rule (make-method/no-data method)
      (lambda (loc #:header [header null])
        (do-method 'method loc header #f)))
    (define/public (do-method method loc header data)
      (define req (request method (->url loc) header data))
      (sync-request req))

    (define/public GET (make-method 'GET))
    (define/public HEAD (make-method/no-data 'HEAD))
    (define/public POST (make-method 'POST))
    (define/public PUT (make-method 'PUT))
    (define/public DELETE (make-method/no-data 'DELETE))
    (define/public OPTIONS (make-method/no-data 'OPTIONS))
    (define/public TRACE (make-method/no-data 'TRACE))

    #;
    (define/public (CONNECT ??) ;; data has "no defined semantics" (4.3.6)
      ??)
    ))

(define http-client%
  (client-mixin connection-manager%))


(define (default-port-for-scheme scheme)
  (case scheme [("http") 80] [("https") 443]))

(define (->url loc) (if (string? loc) (string->url loc) loc))

#;
(begin (define hs '((#"user-agent" #"Racket (http123)") (#"accept-encoding" #"gzip")))
       (define req (request 'GET (string->url "https://www.google.com/") hs #f))
       (define req1 (request 'GET (string->url "http://www.neverssl.com/") hs #f))
       (define c (new http-client%)))

;; FIXME: beware empty paths: (GET "https://www.google.com") fails!
