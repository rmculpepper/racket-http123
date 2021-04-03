#lang racket/base
(provide make-async-exn-input-port
         async-exn-input-port-raise)

(struct async-exn-input-port (in exn-box sema)
  #:property prop:input-port (struct-field-index in))

(define (async-exn-input-port-raise aip e)
  (set-box! (async-exn-input-port-exn-box aip) e)
  (semaphore-post (async-exn-input-port-sema aip))
  (void))

(define (make-async-exn-input-port in)
  (define exn-box (box #f))
  (define sema (make-semaphore 0))
  (define exn-evt
    (wrap-evt (semaphore-peek-evt sema)
              (lambda (_) (raise (unbox exn-box)))))
  (define name (object-name in))
  (define (read-in buf)
    (sync/timeout 0 exn-evt)
    (define r (read-bytes-avail!* buf in))
    (cond [(eqv? r 0)
           (sync (handle-evt in (lambda (_in) (read-in buf)))
                 exn-evt)]
          [else r]))
  (define (peek buf skip progress-evt)
    (sync/timeout 0 exn-evt)
    (define r (peek-bytes-avail!* buf skip progress-evt in))
    (cond [(eqv? r 0)
           (sync (handle-evt in (lambda (_e) (peek buf skip progress-evt)))
                 (handle-evt (or progress-evt never-evt) (lambda (_e) #f))
                 exn-evt)]
          [else r]))
  (define (close)
    (sync/timeout 0 exn-evt)
    ;; FIXME: check for exns again after closing?
    (close-input-port in))
  (define get-progress-evt
    (and (port-provides-progress-evts? in)
         (lambda () (port-progress-evt in))))
  (define (commit k progress-evt done)
    (port-commit-peeked k progress-evt done in))
  (define (get-location)
    (port-next-location in))
  (define (count-lines!)
    (port-count-lines! in))
  (async-exn-input-port
   (make-input-port name read-in peek close
                    get-progress-evt commit
                    get-location count-lines!)
   exn-box
   sema))
