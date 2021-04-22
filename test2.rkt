#lang racket/base
(require racket/match
         racket/class
         rackunit
         "private/interfaces.rkt"
         "private/request.rkt"
         "private/http2.rkt"
         "private/response.rkt"
         "private/h2-frame.rkt")

(define prelude
  (make-parameter
   (list (frame type:SETTINGS 0 0 (fp:settings null))
         (frame type:SETTINGS 1 0 (fp:settings null)))))

;; An Action is Frame | 'sleep | ???.
(define (make-server actions server-in server-out)
  (thread
   (lambda ()
     (parameterize ((current-output-port server-out))
       (for ([action (append (prelude) actions)])
         (match action
           [(? frame? fr)
            (write-frame server-out fr)
            (flush-output server-out)]
           ['sleep (sleep 0.1)]))
       (close-output-port server-out)
       (close-input-port server-in)))))

(define (setup actions)
  (define-values (server-in out-to-server) (make-pipe))
  (define-values (client-in out-to-client) (make-pipe))
  (define server (make-server actions server-in out-to-client))
  (new http2-actual-connection% (in client-in) (out out-to-server) (parent #f)))

(define (test1 expects actions)
  (define ac (setup actions))
  (define r1 (send ac open-request (request 'GET "http://localhost/something" null #f)))
  ((sync r1)))

(define (test1e expects actions)
  (define ac (setup actions))
  (define r1 (send ac open-request (request 'GET "http://localhost/something" null #f)))
  (check-exn (lambda (e)
               (for ([expect expects])
                 (match expect
                   ['raise (raise e)]
                   [(? regexp?) (check regexp-match? expect (exn-message e))]
                   [(? hash?) (for ([(expkey expval) (in-hash expect)])
                                (check-equal? (hash-ref (exn:fail:http123-info e) expkey #f)
                                              expval))])))
             (lambda () ((sync r1)))))

(test1e (list #rx"connection closed by server \\(EOF\\)"
              (hasheq 'code 'server-EOF 'received 'unknown 'version 'http/2))
        (list))

(test1e (list #rx"connection closed by server \\(GOAWAY\\)"
              (hasheq 'code 'GOAWAY 'received 'no 'version 'http/2))
        (list (frame type:GOAWAY 0 0 (fp:goaway 1 error:NO_ERROR #"bye"))))

(test1e (list #rx"connection closed by server \\(EOF\\)"
              (hasheq 'code 'server-EOF 'received 'unknown 'version 'http/2))
        (list (frame type:GOAWAY 0 0 (fp:goaway 3 error:NO_ERROR #"bye"))))

(test1e (list #rx"stream closed by server \\(RST_STREAM\\)"
              (hasheq 'code 'RST_STREAM 'received 'unknown 'version 'http/2))
        (list 'sleep (frame type:RST_STREAM 0 3 (fp:rst_stream error:CONNECT_ERROR))))

(test1e (list #rx"unexpected DATA frame from server"
              (hasheq 'code 'user-agent-stream-error 'version 'http/2))
        (list 'sleep (frame type:DATA 0 3 (fp:data 0 #"abc"))))

(test1e (list #rx"user agent signaled connection error.*error decoding header"
              (hasheq 'code 'user-agent-stream-error 'version 'http/2
                      'received 'yes 'http2-error 'COMPRESSION_ERROR))
        (list 'sleep (frame type:HEADERS (+ flag:END_HEADERS) 3 (fp:headers #f #f #f #"invalid"))))
