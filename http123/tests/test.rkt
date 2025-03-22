;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/class
         racket/port
         racket/system
         racket/runtime-path
         rackunit
         json
         http123
         http123/util/request
         (submod "web-server.rkt" start-server))

;; This tests the client behavior against the Racket web-server.
;; HTTP/1.1 is handled by the Racket web-server directly;
;; HTTP/2 is handled by a nghttpx reverse proxy.

;; See also web-server.rkt and NOTES.md.

;; ----------------------------------------

(define client
  (http-client #:ssl 'auto
               #:add-header
               `([x-racket-always "always"])
               #:add-response-handlers
               `([404 ,(lambda (client resp) 'error)]
                 [502 ,(lambda (client resp)
                         (define content (port->string (send resp get-content-in)))
                         (eprintf "*** 502\n~a\n\n" content)
                         'error-502)]
                 [redirection ,(lambda (client resp) (send client handle-redirection resp))])
               #:add-content-handlers
               `([application/json ,read-json]
                 [application/sexpr ,read]
                 [text/plain ,port->string])))

(define (run-tests prefix)
  (define (hloc suffix) (string-append prefix suffix))
  (define (hreq method suffix [header null] [data #f])
    (request method (hloc suffix) header data))

  ;; ============================================================

  ;; Test 200 response content handlers

  (check-equal? (send client handle (hreq 'GET "/hello-text"))
                "hello world")

  (check-equal? (send client handle (hreq 'POST "/echo" '((content-type "text/plain")) #"abc"))
                "abc")

  (let ([datum '(hello (world) "Racket" 1000)])
    (check-equal? (send client handle
                        (hreq 'POST "/echo" '((content-type "application/sexpr"))
                              (lambda (out) (fprintf out "~s" datum))))
                  datum))

  (check-equal? (send client handle
                      (hreq 'POST "/lots" null #"100"))
                (make-string 100 #\a))

  ;; Test status-code handler (code = 404)
  (check-equal? (send client handle (hreq 'GET "/no-such-location"))
                'error)

  ;; Test unmatched response handler (code = 403)
  (check-exn #rx"handle: no response handler matched"
             (lambda () (send client handle (hreq 'GET "/secret"))))

  ;; Test redirection (and status-class handler)
  (check-equal? (send client handle
                      (hreq 'POST "/redirect301" null
                            (lambda (out) (fprintf out "~a/hello-text" prefix))))
                "hello world")

  ;; ----------------------------------------
  ;; http123/util/request

  (let ([jsexpr (hasheq 'abc 123 'hello "world" 'ns '(1 2 3))])
    (check-equal? (send client handle
                        (request/json
                         'POST (hloc "/echo") null
                         jsexpr))
                  jsexpr))

  (check-equal? (send client handle
                      (request/form-urlencoded
                       'POST (hloc "/form2json") null
                       '((abc . "123") (xyz . #f))))
                (hasheq 'abc "123" 'xyz ""))

  (check-equal? (send client handle
                      (request/multipart
                       'POST (hloc "/form2json") null
                       '((abc "123") (xyz "") (somefile "" () #:filename "empty.txt"))))
                (hasheq 'abc "123" 'xyz ""))

  (void))

;; ----------------------------------------

(when #t
  (define prefix "http://localhost:17180")
  (printf "Running tests for HTTP/1.1, prefix=~s\n" prefix)
  (run-tests prefix))

(when have-http2?
  (define prefix "https://localhost:17190")
  (printf "Running tests for HTTP/2, prefix=~s\n" prefix)
  (run-tests prefix))

(shutdown-servers)
