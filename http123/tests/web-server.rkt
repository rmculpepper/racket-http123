;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/runtime-path
         racket/tcp
         web-server/servlet
         web-server/servlet-env
         json)
(provide (all-defined-out))

(define-values (dispatch _make-url)
  (dispatch-rules
   [("hello-text") #:method "get"
    (lambda (req) (ok-response #"text/plain" "hello world"))]
   ;; ----
   [("echo") #:method "post"
    (lambda (req)
      (define content-type
        (cond [(headers-assq* #"content-type" (request-headers/raw req)) => header-value]
              [else #"content-type" #"text/unknown"]))
      (define body (request-post-data/raw req))
      (response/full
       200 #"Found"
       (current-seconds) content-type
       null
       (list body)))]
   [("lots") #:method "post"
    (lambda (req)
      (define body (request-post-data/raw req))
      (define n (read (open-input-bytes body)))
      (response/output
       #:code 200
       #:mime-type #"text/plain"
       (lambda (out) (for ([i n]) (write-char #\a out)))))]
   [("slow" (integer-arg))
    (lambda (req delay-ms)
      (response/output
       #:code 200
       #:mime-type #"text/plain"
       (lambda (out)
         (for ([i #e1e2])
           (sleep (* 0.001 delay-ms))
           (for ([j 10]) (write-char #\a out))
           (flush-output out)))))]
   [("secret") #:method "get"
    (lambda (req)
      (response/output
       #:code 403
       #:mime-type #"text/plain"
       (lambda (out) (fprintf out "go away!"))))]
   [("redirect301") #:method "post"
    (lambda (req)
      (response/output
       #:code 301
       #:headers (list (header #"Location" (request-post-data/raw req)))
       void))]
   [("form2json") #:method "post"
    (lambda (req)
      (response/output
       #:code 200
       #:mime-type #"application/json"
       (lambda (out)
         (write-json (for/hash ([b (request-bindings/raw req)] #:when (binding:form? b))
                       (values (string->symbol (bytes->string/utf-8 (binding-id b)))
                               (bytes->string/utf-8 (binding:form-value b))))
                     out))))]
   ))

(define (ok-response content-type data)
  (response/output
   #:code 200
   #:mime-type content-type
   (lambda (out)
     (cond [(string? data) (write-string data out)]
           [(procedure? data) (data out)]))))

;; --

(define-runtime-path static-dir "static")

;; Go

(define (start [log? #f])
  (serve/servlet dispatch
                 #:port 17180
                 #:servlet-regexp #rx""
                 #:command-line? #t
                 ;; #:launch-browser? #f
                 #:extra-files-paths (list (path->string static-dir))
                 #:log-file (if log? "/dev/stdout" #f)))

(define (wait-for-port port)
  (define WAIT-MS 10000.0)
  (define SLEEP-S 0.1)
  (define stop (+ WAIT-MS (current-inexact-milliseconds)))
  (let loop ()
    (define port-ready?
      (with-handlers ([exn:fail:network? (lambda (e) #f)])
        (define-values (in out) (tcp-connect "localhost" port))
        (begin (close-input-port in) (close-output-port out) #t)))
    (unless port-ready?
      (cond [(< (current-inexact-milliseconds) stop)
             (begin (sleep SLEEP-S) (loop))]
            [else (error 'wait-for-port "port ~s not ready after ~s ms" port WAIT-MS)]))))

;; ----------------------------------------

(module+ start-server
  (require racket/system
           racket/port)
  (provide (all-defined-out))

  (define server-cust (make-custodian))

  (define (shutdown-servers)
    (custodian-shutdown-all server-cust))

  ;; Start the Racket web server
  (parameterize ((current-custodian server-cust))
    (void (thread start)))
  (wait-for-port 17180)

  ;; Start the nghttpx reverse proxy, if available.
  (define nghttpx (find-executable-path "nghttpx"))
  (define-runtime-path key-pem "key.pem")
  (define-runtime-path cert-pem "cert.pem")
  (when nghttpx
    (parameterize ((current-custodian server-cust)
                   (current-subprocess-custodian-mode 'interrupt)
                   ;; Discard nghttpx logging
                   #;(current-error-port (open-output-nowhere)))
      (void
       (thread
        (lambda ()
          (system* nghttpx
                   "-b" "localhost,17180"
                   "-f" "*,17190"
                   "--no-ocsp"
                   key-pem cert-pem)))))
    (wait-for-port 17190))
  (define have-http2? (and nghttpx #t)))

(module+ main
  (require (submod ".." start-server))
  (sync never-evt))
