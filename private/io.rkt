#lang racket/base
(require ffi/unsafe/atomic
         racket/tcp
         openssl)
(provide (except-out (all-defined-out)
                     unsafe-wrap-input-port)
         (protect-out unsafe-wrap-input-port))

;; ----------------------------------------

(struct box-evt (b sema evt)
  #:property prop:evt (struct-field-index evt))

(define (make-box-evt [call? #f])
  (define b (box #f))
  (define sema (make-semaphore 0))
  (define evt ;; FIXME: wrap or handle?
    (wrap-evt (semaphore-peek-evt sema)
              (if call?
                  (lambda (_e) ((unbox b)))
                  (lambda (_e) (unbox b)))))
  (box-evt b sema evt))

(define (box-evt-set! be v)
  (set-box! (box-evt-b be) v)
  (void (semaphore-post (box-evt-sema be))))

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

(struct wrapped-input-port (in exnbe)
  #:property prop:input-port (struct-field-index in))

(define (wrapped-input-port-raise aip e)
  (void (box-evt-set! (wrapped-input-port-exnbe aip) e)))

(define (wrap-input-port in #:on-consume [on-consume void])
  (unsafe-wrap-input-port in #:on-consume on-consume))

(define (unsafe-wrap-input-port in
                                #:on-consume [on-consume void]
                                #:atomic-on-consume [atomic-on-consume void])
  (define exnbe (make-box-evt))
  (define exn-evt (wrap-evt exnbe raise))
  (define name (object-name in))
  ;; peek counts as consume, but don't double-count w/ other peek/write
  (define peeked 0)
  (define (read-in buf)
    (sync/timeout 0 exn-evt)
    (start-atomic)
    (define r (read-bytes-avail!* buf in))
    (cond [(eqv? r 0)
           (end-atomic)
           (choice-evt (handle-evt in (lambda (_in) (read-in buf)))
                       exn-evt)]
          [else
           (cond [(exact-nonnegative-integer? r)
                  (cond [(<= r peeked)
                         (set! peeked (- peeked r))
                         (end-atomic)]
                        [else
                         (set! peeked 0)
                         (atomic-on-consume (- r peeked))
                         (end-atomic)
                         (on-consume (- r peeked))])]
                 [else (end-atomic) (void)])
           r]))
  (define (peek buf skip progress-evt)
    (sync/timeout 0 exn-evt)
    (start-atomic)
    (define r (peek-bytes-avail!* buf skip progress-evt in))
    (cond [(eqv? r 0)
           (end-atomic)
           (choice-evt (handle-evt in (lambda (_e) (peek buf skip progress-evt)))
                       (handle-evt (or progress-evt never-evt) (lambda (_e) #f))
                       exn-evt)]
          [else
           (cond [(exact-nonnegative-integer? r)
                  (define new-peeked (+ skip r))
                  (cond [(> new-peeked peeked)
                         (define diff (- new-peeked peeked))
                         (set! peeked new-peeked)
                         (atomic-on-consume diff)
                         (end-atomic)
                         (on-consume diff)]
                        [else (end-atomic) (void)])]
                 [else (end-atomic) (void)])
           r]))
  (define abandon-box (box #f))
  (define (close)
    (if (unbox abandon-box)
        (abandon-port in)
        (close-input-port in)))
  (define get-progress-evt
    (and (port-provides-progress-evts? in)
         (lambda () (port-progress-evt in))))
  (define (commit k progress-evt done)
    ;; Note: not safe to call port-commit-peeked in atomic mode, in general.
    (cond [(port-commit-peeked k progress-evt done in)
           => (lambda (r) (start-atomic) (set! peeked (max 0 (- peeked k))) (end-atomic) r)]
          [else #f]))
  (define (get-location)
    (port-next-location in))
  (define (count-lines!)
    (port-count-lines! in))
  (define port
    (wrapped-input-port
     (make-input-port name read-in peek close
                      get-progress-evt commit
                      get-location count-lines!)
     exnbe))
  (when (port-with-abandon? in)
    (define (abandon p) (set-box! abandon-box #t) (close-output-port p))
    (hash-set! abandon-table port abandon))
  port)

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
    (cond [(< start end)
           (write-bytes buf tmp start end)
           (- end start)]
          [else ;; flush => non-block = #f (allowed to block)
           (begin (flush) 0)]))
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
  (define (close-or-abandon abandon?)
    (cond [abandon?
           (DEBUG (eprintf "** abandoning\n"))
           (flush)
           (abandon-port out)]
          [else
           (DEBUG (eprintf "** closing\n"))
           (flush)
           (close-output-port out)]))
  (define abandon-box (box #f))
  (define close
    (if (port-with-abandon? out)
        (lambda () (close-or-abandon (unbox abandon-box)))
        (lambda () (close-or-abandon #f))))
  (define port
    (make-output-port (object-name out)
                      always-evt
                      write-out
                      close))
  (when (port-with-abandon? out)
    (define (abandon p) (set-box! abandon-box #t) (close-output-port p))
    (hash-set! abandon-table port abandon))
  port)
