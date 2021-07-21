#lang racket/base
(require racket/class
         rackunit
         json
         "main.rkt")

;; Test using a few real live web servers.

(define client
  (http-client #:add-header
               `([x-racket-version ,(version)])
               #:add-content-handlers
               `([application/json ,read-json])))

(test-case "data.jsontest.com"
  (define time-req (request 'GET "http://date.jsontest.com/"))
  (define result (send client handle time-req))
  (check-pred jsexpr? result))

(test-case "fail ietf.org"
  (check-exn #rx"no content handler matched"
             (lambda ()
               (send client handle (request 'GET "https://tools.ietf.org/rfc/rfc7540.txt")))))

(define client2
  (send client fork
        #:add-content-handlers
        `([text/plain ,(lambda (in) (read-string 40 in))]
          [*/* ,(lambda (in)
                  (format "something called ~s, I guess"
                          (send (current-response) get-content-type)))])))

(test-case "ok ietf"
  (check-pred string?
              (send client2 handle (request 'GET "https://tools.ietf.org/rfc/rfc7540.txt"))))
(test-case "ok google"
  (check-regexp-match #rx"something called text/html, I guess"
                      (send client2 handle (request 'GET "https://www.google.com/"))))

(test-case "fail mirror"
  (check-exn #rx"no response handler matched"
             (lambda ()
               (send client2 handle
                     (request 'GET "https://mirror.racket-lang.org/no-such-file.html")))))

(define client3
  (send client2 fork
        #:add-response-handlers
        `([404 ,(lambda (client resp) 'not-found)]
          [client-error ,(lambda (client resp) 'failed)])))

(test-case "ok 403"
  (check-equal? (send client3 handle (request 'GET "https://racket-lang.org/secret-plans.scrbl"))
                'failed))
(test-case "ok 404"
  (check-equal? (send client3 handle
                      (request 'GET "https://mirror.racket-lang.org/no-such-file.html"))
                'not-found))
