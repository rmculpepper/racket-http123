;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/class
         racket/match
         rackunit
         "private/interfaces.rkt"
         "private/response.rkt"
         "private/http11.rkt"
         "private/request.rkt"
         (prefix-in util: "private/util.rkt"))

(define (make-server responses server-in server-out)
  (thread
   (lambda ()
     (parameterize ((current-output-port server-out))
       (for ([resp responses])
         (match resp
           [(list* code flags)
            (printf "HTTP/1.1 ~a Fake response\r\n" code)
            (unless (memq 'eof-after-status flags)
              (printf "Key1: Value1\r\n")
              (when (memq 'bad-header flags)
                (printf "Bad Key: Value\r\n"))
              (when (memq 'close flags)
                (printf "Connection: close\r\n"))
              (cond [(= code 200)
                     (printf "Content-Length: 6\r\n\r\n")
                     (unless (memq 'eof-before-content flags)
                       (printf "hello\n"))]
                    [else
                     (printf "Content-Length: 0\r\n\r\n")]))]
           ['sleep (sleep 0.1)])
         (flush-output server-out))
       (close-output-port server-out)
       (close-input-port server-in)))))

(define fake-parent%
  (class object%
    (super-new)

    (define/public (url->host-bytes u)
      (util:url->host-bytes u))

    (define/public (on-actual-disconnect ac)
      (void))
    ))
(define parent (new fake-parent%))

(define (test responses flags r2-rx)
  (define-values (server-in out-to-server) (make-pipe))
  (define-values (client-in out-to-client) (make-pipe))
  (define server (make-server responses server-in out-to-client))
  (define ac
    (new http11-actual-connection%
         (in client-in) (out out-to-server) (parent parent)))
  (define r1 (send ac open-request (request 'GET "http://localhost/something" null #f)))
  (sleep 0.05)
  (define r2 (send ac open-request (request 'GET "http://localhost/another" null #f)))
  (check-pred evt? r1)
  (cond [(memq 'no-r2 flags)
         (check-eq? r2 #f)]
        [else
         (check-pred evt? r2)])
  (check-pred (lambda (v) (is-a? v http11-response%)) ((sync r1)))
  (unless (memq 'no-r2 flags)
    (with-handlers ([(lambda (e) #t)
                     (lambda (e)
                       (check-pred exn:fail:http123? e)
                       (check regexp-match? r2-rx (exn-message e))
                       (define info (exn:fail:http123-info e))
                       (when (memq 'no-received flags)
                         (check-eq? (hash-ref info 'received #f) 'no))
                       (when (memq 'yes-received flags)
                         (check-eq? (hash-ref info 'received #f) 'yes))
                       (when (memq 'unknown-received flags)
                         (check-eq? (hash-ref info 'received #f) 'unknown))
                       (void))])
      ((sync r2))
      (error "failed to raise exn"))))

(test '((200))
      '(no-r2) #f)

(test '(sleep (200))
      '(unknown-received) #rx"connection closed by server")

(test '(sleep (200) (200 eof-after-status))
      '(yes-received) #rx"error reading response from server")

(test '(sleep (200) (200 bad-header))
      '(yes-received) #rx"error reading response from server.*malformed header field line")

(test '(sleep (200) (200 eof-before-content))
      '(yes-received) #rx"error reading response from server")

(test '(sleep (200 close))
      '(no-received) #rx"connection closed by server")
