#lang racket/base
(require racket/class
         racket/promise
         http123
         rackunit
         (submod "web-server.rkt" start-server))

(define c (http-client #:ssl 'auto))

(define DELAY 300) ;; ms
(define ITERS 100) ;; see web-server.rkt

(printf "Expected test time is slightly over ~s s\n"
        (quotient (* DELAY ITERS) 1000))

(define req-url (format "https://localhost:17190/slow/~a" DELAY))

(define NTHREADS 20)

(define (worker)
  ;; Measure maximum delay, time spent blocking on read.
  (define r (send c sync-request (request 'GET req-url)))
  (define in (send r get-content-in))
  (define buffer (make-bytes 100))
  (let loop ([maxdelay 0])
    (define start (current-inexact-milliseconds))
    (define result (read-bytes-avail! buffer in))
    (define delay (- (current-inexact-milliseconds) start))
    (define maxdelay* (max maxdelay delay))
    (cond [(eof-object? result) maxdelay*]
          [else (loop maxdelay*)])))

(define start (current-inexact-milliseconds))
(define result-ps
  (for/list ([i NTHREADS])
    (delay/thread (worker))))
(define results
  (map force result-ps))
(define total-time (- (current-inexact-milliseconds) start))

results
total-time

(test-case "max read delay"
  ;; Generous slack because thread scheduling can be noisy.
  (define ITER-SLACK 1.30)
  (for ([result results])
    (check < result (* DELAY ITER-SLACK))))

(test-case "total time"
  ;; This tests that all NTHREADS clients are served concurrently.
  (define TOTAL-SLACK 1.05)
  (check < total-time (* DELAY ITERS TOTAL-SLACK)))

(shutdown-servers)
