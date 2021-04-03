#lang racket/base
(require racket/tcp
         openssl)
(provide (all-defined-out))

;; ----------------------------------------

(struct box-evt (b sema evt)
  #:property prop:evt (struct-field-index evt))

(define (make-box-evt [call? #f])
  (define b (box #f))
  (define sema (make-semaphore 0))
  (define evt
    (wrap-evt (semaphore-peek-evt sema)
              (lambda (_e) (if call? ((unbox b)) (unbox b)))))
  (box-evt b sema evt))

(define (box-evt-set! be v)
  (set-box! (box-evt-b be) v)
  (void (semaphore-post (box-evt-sema be))))

;; ----------------------------------------

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

;; ----------------------------------------

;; Warning: this might interact strangely with finalization...
(define abandon-table (make-weak-hasheq))

(define (port-with-abandon? v)
  (or (tcp-port? v) (ssl-port? v) (hash-has-key? abandon-table v)))

(define (abandon-port p)
  (cond [(tcp-port? p) (tcp-abandon-port p)]
        [(ssl-port? p) (ssl-abandon-port p)]
        [(hash-ref abandon-table p #f)
         => (lambda (abandon) (abandon p))]
        [else (error 'abandon-port "cannot abandon port: ~e" p)]))

;; ----------------------------------------

;; adapted from db/private/postgresql/connection.rkt
;; SSL output ports currently seem to create one SSL record per port write.
;; This causes an explosion in the amount of data sent, due to padding, HMAC,
;; and other record overhead. So add an ad hoc buffering port around it.
(define (buffering-output-port out)
  (define-syntax-rule (DEBUG expr ...) (when #f expr ...))
  (DEBUG (eprintf "** making buffered output port\n"))
  (define tmp (open-output-bytes))
  (define (write-out buf start end non-block? eb?)
    (when (> end start) (write-bytes buf tmp start end))
    (when (= end start) (flush))
    (- end start))
  (define (flush)
    (define buf (get-output-bytes tmp #t))
    (define end (bytes-length buf))
    (DEBUG (eprintf "** flushing ~s bytes\n" end))
    ;; SSL seems to have trouble accepting too much data at once...
    (define CHUNK (expt 2 22))
    (for ([start (in-range 0 end CHUNK)])
      (DEBUG (eprintf "-- flushing range [~s,~s)\n" start (min end (+ start CHUNK))))
      (write-bytes buf out start (min end (+ start CHUNK)))
      (flush-output out))
    (when (zero? end) (flush-output out)))
  (define abandon-box (box #f))
  (define (close)
    (cond [(unbox abandon-box)
           (DEBUG (eprintf "** abandoning\n"))
           (flush)
           (abandon-port out)]
          [else
           (DEBUG (eprintf "** closing\n"))
           (flush)
           (close-output-port out)]))
  (define port
    (make-output-port (object-name out)
                      always-evt
                      write-out
                      close))
  (when (port-with-abandon? out)
    (define (abandon p) (set-box! abandon-box #t) (close-output-port p))
    (hash-set! abandon-table port abandon))
  port)
