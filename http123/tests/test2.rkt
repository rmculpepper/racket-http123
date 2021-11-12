;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require (for-syntax racket/base)
         racket/match
         racket/class
         syntax/srcloc
         rackunit
         http123
         http123/private/http2
         http123/private/h2-frame
         http123/private/hpack)

;; Test various error conditions for HTTP/2 actual connections using a
;; fake server that sends scripted sequences of frames.

(define TEST-TIMEOUT? #f)

(define prelude
  (make-parameter
   (list (frame type:SETTINGS 0 0 (fp:settings null))
         (frame type:SETTINGS 1 0 (fp:settings null))
         'sleep)))

;; An Action is Frame | 'sleep | Real | ???.
(define (make-server actions server-in server-out)
  (thread
   (lambda ()
     (parameterize ((current-output-port server-out))
       (for ([action (append (prelude) actions)])
         (match action
           [(? frame? fr)
            (write-frame server-out fr)
            (flush-output server-out)]
           [(list 'buggy (? exact-integer? adjustlen) (? frame? fr))
            (buggy-write-frame server-out fr adjustlen)
            (flush-output server-out)]
           [(? real? t) (sleep t)]
           ['sleep (sleep 0.02)]))
       (close-output-port server-out)
       (close-input-port server-in)))))

(define (buggy-write-frame out fr adjustlen)
  (match-define (frame type flags streamid payload) fr)
  (write-frame-header out (+ adjustlen (payload-length flags payload)) type flags streamid)
  (write-frame-payload out flags payload))

(define (setup actions)
  (define-values (server-in out-to-server) (make-pipe))
  (define-values (client-in out-to-client) (make-pipe))
  (define server (make-server actions server-in out-to-client))
  (new http2-actual-connection% (in client-in) (out out-to-server) (parent #f)))

(begin-for-syntax
  (define ((make-test-case-wrapper proc-id) stx)
    (syntax-case stx ()
      [(_ arg ...)
       #`(test-case (source-location->string (quote-syntax #,stx))
           (#,proc-id arg ...))])))

;; ============================================================
;; Error tests

(define (run-expects expects e)
  (for ([expect expects])
    (match expect
      ['raise (raise e)]
      [(? regexp?) (check regexp-match? expect (exn-message e))]
      [(? hash?) (for ([(expkey expval) (in-hash expect)])
                   (check-equal? (hash-ref (exn:fail:http123-info e) expkey #f)
                                 expval))])))

(define (test1e* expects actions)
  (define ac (setup actions))
  (define r1 (send ac open-request (request 'GET "https://localhost/something" null #f)))
  (check-exn (lambda (e) (run-expects expects e))
             (lambda () ((sync r1)))))

(define-syntax test1e (make-test-case-wrapper #'test1e*))

(define (test1ok* proc actions)
  (define ac (setup actions))
  (define r1 (send ac open-request (request 'GET "https://localhost/something" null #f)))
  (void (proc ((sync r1)))))

(define-syntax test1ok (make-test-case-wrapper #'test1ok*))

(define conn-err (hasheq 'code 'ua-connection-error 'version 'http/2))
(define conn-proto-err (hash-set conn-err 'http2-error 'PROTOCOL_ERROR))
(define stream-err (hasheq 'code 'ua-stream-error 'version 'http/2))
(define stream-proto-err (hash-set stream-err 'http2-error 'PROTOCOL_ERROR))

;; ----------------------------------------
;; from http2.rkt:

;; handle-frame-or-other:
(test1e (list #rx"frame size error")
        (list (frame type:DATA 0 3 (fp:data 0 (make-bytes (add1 DEFAULT-MAX-FRAME-SIZE))))))
(test1e (list #rx"bad padding")
        (list (list 'buggy -50 (frame type:DATA flag:PADDED 3 (fp:data 100 #"")))))

;; get-stream:
(test1e (list #rx"reference to stream 0" conn-proto-err)
        (list (frame type:HEADERS flag:END_HEADERS 0 (fp:headers #f #f #f #""))))
(test1e (list #rx"unknown stream" conn-err (hasheq 'http2-error 'STREAM_CLOSED))
        (list (frame type:DATA 0 99 (fp:data 0 #"abc"))))

;; handle-frame:
(test1e (list #rx"expected CONTINUATION frame" conn-proto-err)
        (list (frame type:HEADERS 0 3 (fp:headers #f #f #f #""))
              (frame type:DATA 0 3 (fp:data 0 #"abc"))))

;; handle-multipart-frame:
(test1e (list #rx"error decoding header" conn-err
              (hasheq 'received 'yes 'http2-error 'COMPRESSION_ERROR))
        (list (frame type:HEADERS flag:END_HEADERS 3 (fp:headers #f #f #f #"bad"))))

;; handle-frame:
(test1e (list #rx"requires stream 0" conn-proto-err)
        (list (frame type:PING 0 3 (fp:ping (make-bytes 8 0)))))
(test1e (list #rx"requires stream 0" conn-proto-err)
        (list (frame type:GOAWAY 0 3 (fp:goaway 3 error:NO_ERROR #"bye"))))
(test1e (list #rx"reference to stream 0" conn-proto-err)
        (list (frame type:DATA 0 0 (fp:data 0 #"abc"))))
(test1e (list #rx"non-empty SETTINGS ack" conn-err (hasheq 'http2-error 'FRAME_SIZE_ERROR))
        (list (frame type:SETTINGS flag:ACK 0 (fp:settings (list (setting 'enable-push 0))))))
(test1e (list #rx"unexpected SETTINGS ack" conn-proto-err)
        (list (frame type:SETTINGS flag:ACK 0 (fp:settings (list)))))
(test1e (list #rx"push not enabled" conn-proto-err)
        (list (frame type:PUSH_PROMISE flag:END_HEADERS 3 (fp:push_promise 0 6 #""))))
(test1e (list #rx"unexpected CONTINUATION frame" conn-proto-err)
        (list (frame type:CONTINUATION 0 3 (fp:continuation #""))))

;; handle-settings:
(test1e (list #rx"bad enable_push value" conn-proto-err)
        (list (frame type:SETTINGS 0 0 (fp:settings (list (setting 'enable-push 99))))))
(test1e (list #rx"window too large" conn-err (hasheq 'http2-error 'FLOW_CONTROL_ERROR))
        (list (frame type:SETTINGS 0 0
                     (fp:settings (list (setting 'initial-window-size (sub1 (expt 2 32))))))))

;; adjust-out-flow-window:
(test1e (list #rx"window too large" conn-err (hasheq 'http2-error 'FLOW_CONTROL_ERROR))
        (list (frame type:WINDOW_UPDATE 0 0 (fp:window_update (sub1 (expt 2 31))))))

;; timeout
(when TEST-TIMEOUT?
  (test1e (list #rx"connection closed by user agent \\(timeout\\)"
                (hasheq 'code 'ua-timeout))
          (list 60))
  (test1e (list 'raise #rx"connection closed by server (RST_STREAM)"
                (hasheq 'code 'server-reset-stream 'http2-error 'NO_ERROR))
          (list 12 (frame type:PING flag:ACK 0 (fp:ping (make-bytes 8 0)))
                10 (frame type:RST_STREAM 0 3 (fp:rst_stream error:NO_ERROR)))))

;; ----------------------------------------
;; from h2-stream:

;; adjust-out-flow-window:
(test1e (list stream-err (hasheq 'http2-error 'FLOW_CONTROL_ERROR))
        (list (frame type:WINDOW_UPDATE 0 3 (fp:window_update (sub1 (expt 2 31))))))

;; check-state:
;; FIXME

;; handle-user-abort:
;; FIXME

;; make-header-promise:
(define (new-dt) (make-dtable 4096))
(define (mkheaders hs) (fp:headers #f #f #f (encode-header hs (new-dt))))

(test1e (list #rx"bad or missing status from server")
        (list (frame type:HEADERS flag:END_HEADERS 3 (mkheaders '((#"bad key" #"value"))))))

(test1e (list #rx"error processing header")
        (list (frame type:HEADERS flag:END_HEADERS 3
                     (mkheaders '((#":status" #"200") (#"bad key" #"value"))))))

(test1ok (lambda (r) 'ok)
         (list (frame type:HEADERS (+ flag:END_STREAM flag:END_HEADERS) 3
                      (mkheaders '((#":status" #"200" never-add) (#"key" #"value" never-add))))))

;; pstate-base% bad-tx:
(test1e (list #rx"unexpected DATA frame"
              stream-proto-err)
        (list (frame type:DATA 0 3 (fp:data 0 #"abc"))))

;; pstate-base% handle-rst_stream:
(test1e (list #rx"stream closed by server \\(RST_STREAM\\)"
              (hasheq 'version 'http/2 'code 'server-reset-stream 'received 'unknown
                      'http2-error 'ENHANCE_YOUR_CALM))
        (list (frame type:RST_STREAM 0 3 (fp:rst_stream error:ENHANCE_YOUR_CALM))))
(test1e (list #rx"stream closed by server \\(RST_STREAM\\)"
               (hasheq 'version 'http/2 'code 'server-reset-stream 'received 'unknown
                       'http2-error 'CONNECT_ERROR))
        (list (frame type:RST_STREAM 0 3 (fp:rst_stream error:CONNECT_ERROR))))

(test1e (list #rx"connection closed by server \\(GOAWAY\\)"
              (hasheq 'version 'http/2 'code 'server-closed 'received 'no 'http2-error 'NO_ERROR))
        (list (frame type:GOAWAY 0 0 (fp:goaway 1 error:NO_ERROR #"bye"))))

(test1e (list #rx"connection closed by server \\(EOF\\)"
              (hasheq 'code 'server-EOF 'received 'unknown 'version 'http/2))
        (list))

(test1e (list #rx"connection closed by server \\(EOF\\)"
              (hasheq 'code 'server-EOF 'received 'unknown 'version 'http/2))
        ;; Note: last-streamid = 3 means stream 3 won't get the GOAWAY.
        (list (frame type:GOAWAY 0 0 (fp:goaway 3 error:NO_ERROR #"bye"))))

;; expect-header-pstate%:
(test1e (list #rx"bad or missing status"
              (hasheq 'version 'http/2 'code 'bad-status 'received 'yes))
        (list (frame type:HEADERS flag:END_HEADERS 3 (mkheaders null))))
(test1e (list #rx"bad or missing status"
              (hasheq 'version 'http/2 'code 'bad-status 'received 'yes))
        (list (frame type:HEADERS flag:END_HEADERS 3 (mkheaders '((#":status" #"fine"))))))
(test1e (list #rx"bad or missing status"
              (hasheq 'version 'http/2 'code 'bad-status 'received 'yes))
        (list (frame type:HEADERS flag:END_HEADERS 3 (mkheaders '((#":status" #"892"))))))

;; reading-response-pstate%
;; FIXME

;; ============================================================
;; Error tests for errors sending data

(define (test1de* expects actions #:data data)
  (define ac (setup actions))
  (define r (send ac open-request (request 'GET "https://localhost/something" null data)))
  (check-exn (lambda (e) (run-expects expects e))
             (lambda () ((sync r)))))
(define-syntax test1de (make-test-case-wrapper #'test1de*))

(test1de (list #rx"request canceled by exception from data procedure"
               stream-err (hasheq 'http2-error 'CANCEL 'received 'unknown))
         (list)
         #:data (lambda (put)
                  (put #"hello")
                  (error 'nevermind)))

;; ============================================================

(define (test1r* hh actions #:data [data #f])
  (define ac (setup actions))
  (define r ((sync (send ac open-request (request 'GET "https://localhost/" null data)))))
  r)

(define-syntax test1r (make-test-case-wrapper #'test1r*))
