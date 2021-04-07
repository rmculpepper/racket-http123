#lang racket/base
(require ffi/unsafe/atomic
         racket/tcp
         openssl)
(provide (all-defined-out))

;; ============================================================

;; Warning: this might interact strangely with finalization...
(define abandon-table (make-weak-hasheq))

;; Abandonability is not automatically propagated through
;; prop:input-port and prop:output-port.

(define (port-with-abandon? v)
  (or (tcp-port? v) (ssl-port? v) (hash-has-key? abandon-table v)))

(define (abandon-port p)
  (cond [(tcp-port? p) (tcp-abandon-port p)]
        [(ssl-port? p) (ssl-abandon-port p)]
        [(hash-ref abandon-table p #f)
         => (lambda (abandon) (abandon p))]
        [else (error 'abandon-port "cannot abandon port: ~e" p)]))

;; ============================================================

(define (make-input-port* #:name name
                          #:read-in read-in
                          #:peek peek
                          #:close close
                          #:abandon [abandon #f]
                          #:get-progress-evt [get-progress-evt #f]
                          #:commit [commit #f]
                          #:get-location [get-location #f]
                          #:count-lines! [count-lines! void]
                          #:init-position [init-position 1]
                          #:buffer-mode [buffer-mode #f])
  (define (make close)
    (make-input-port name read-in peek close
                     get-progress-evt commit get-location
                     count-lines! init-position buffer-mode))
  (cond [(procedure? abandon)
         (define abandon-cell (make-thread-cell #f #f))
         (define (close/check-for-abandon)
           (if (thread-cell-ref abandon-cell)
               (abandon)
               (close)))
         (define port (make close/check-for-abandon))
         (define (do-abandon p)
           (thread-cell-set! abandon-cell #t) (close-input-port))
         (hash-set! abandon-table port do-abandon)]
        [else (make close)]))

(define (make-output-port* #:name name
                           #:evt evt
                           #:write-out write-out
                           #:close close
                           #:abandon [abandon #f]
                           #:write-out-special [write-out-special #f]
                           #:get-write-evt [get-write-evt #f]
                           #:get-write-special-evt [get-write-special-evt #f]
                           #:get-location [get-location #f]
                           #:count-lines! [count-lines! void]
                           #:init-position [init-position 1]
                           #:buffer-mode [buffer-mode #f])
  (define (make close)
    (make-output-port name evt write-out close
                      write-out-special get-write-evt get-write-special-evt
                      get-location count-lines! init-position buffer-mode))
  (cond [(procedure? abandon)
         (define abandon-cell (make-thread-cell #f #f))
         (define (close/check-for-abandon)
           (if (thread-cell-ref abandon-cell)
               (abandon)
               (close)))
         (define port (make close/check-for-abandon))
         (define (do-abandon p)
           (thread-cell-set! abandon-cell #t) (close-output-port))
         (hash-set! abandon-table port do-abandon)]
        [else (make close)]))

;; ============================================================

(struct box-evt (b sema evt)
  #:property prop:evt (struct-field-index evt))
(define box-is-unset (gensym))

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
  (match-define (box-evt b sema evt) be)
  (cond [(eq? (unbox b) box-is-unset)
         (set-box! (box-evt-b be) v)
         (void (semaphore-post (box-evt-sema be)))
         #t]
        [else #f]))

;; ----------------------------------------

(struct wrapped-input-port (in exnbe)
  #:property prop:input-port (struct-field-index in))

(define (wrapped-input-port-raise aip e)
  (void (box-evt-set! (wrapped-input-port-exnbe aip) e)))

;; FIXME: should reader receive exn immediately or in place of EOF?
(define (wrap-input-port in #:delay-to-eof? [delay-to-eof? #f])
  (define exnbe (make-box-evt))
  (define exn-evt (wrap-evt exnbe raise))
  ;; FIXME: could implement check directly, instead of via sync/timeout
  (define (check-early) (unless delay-to-eof? (sync/timeout 0 exn-evt)))
  (define (check-at-eof) (sync/timeout 0 exn-evt))
  (define name (object-name in))
  (define (read-in buf)
    (check-early)
    (define r (read-bytes-avail!* buf in))
    (cond [(eqv? r 0)
           (choice-evt (handle-evt in (lambda (_in) (read-in buf)))
                       exn-evt)]
          [(eof-object? r) (check-at-eof) r]
          [else r]))
  (define (peek buf skip progress-evt)
    (check-early)
    (define r (peek-bytes-avail!* buf skip progress-evt in))
    (cond [(eqv? r 0)
           (choice-evt (handle-evt in (lambda (_e) (peek buf skip progress-evt)))
                       (handle-evt (or progress-evt never-evt) (lambda (_e) #f))
                       exn-evt)]
          [(eof-object? r) (check-at-eof) r]
          [else r]))
  (define (close) (close-input-port))
  (define (abandon) (abandon-port in))
  (define get-progress-evt
    (and (port-provides-progress-evts? in)
         (lambda () (port-progress-evt in))))
  (define (commit k progress-evt done)
    (check-early)
    (port-commit-peeked k progress-evt done in))
  (define (get-location)
    (port-next-location in))
  (define (count-lines!)
    (port-count-lines! in))
  (wrapped-input-port
   (make-input-port* #:name name
                     #:read-in read-in
                     #:peek peek
                     #:close close
                     #:abandon (and (port-with-abandon? in) abandon)
                     #:get-progress-evt get-progress-evt commit
                     #:get-location get-location
                     #:count-lines1 count-lines!)
   exnbe))

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
  (define (close) (close-output-port out))
  (define (abandon) (abandon-port out))
  (make-output-port* #:name (object-name out)
                     #:evt always-evt
                     #:write-out write-out
                     #:close close
                     #:abandon (and (port-with-abandon? out) abandon)))
