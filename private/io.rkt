;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/match
         racket/tcp
         openssl)
(provide (all-defined-out))

;; ============================================================

(define (sleep-evt sec)
  (guard-evt
   (lambda ()
     (alarm-evt
      (+ (current-inexact-milliseconds) (* 1000.0 sec))))))

;; ============================================================

;; Warning: this might interact strangely with finalization...
(define abandon-table (make-weak-hasheq))

;; Abandonability is not automatically propagated through
;; prop:input-port and prop:output-port.

(define (port-with-abandon? v)
  (or (tcp-port? v) (ssl-port? v) (hash-has-key? abandon-table v)))

(define (abandon-port p [or-close? #t])
  (cond [(tcp-port? p) (tcp-abandon-port p)]
        [(ssl-port? p) (ssl-abandon-port p)]
        [(hash-ref abandon-table p #f)
         => (lambda (abandon) (abandon p))]
        [or-close?
         (when (output-port? p) (close-output-port p))
         (when (input-port? p) (close-input-port p))]
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
           (thread-cell-set! abandon-cell #t) (close-output-port p))
         (hash-set! abandon-table port do-abandon)]
        [else (make close)]))

;; ============================================================

;; BoxEvt[X] = (box-evt (box (U X #f)) Semaphore Semaphore Evt[X])
(struct box-evt (b wsema rsema evt)
  #:property prop:evt (struct-field-index evt))

(define (make-box-evt)
  (define b (box #f))
  (define wsema (make-semaphore 1))
  (define rsema (make-semaphore 0))
  (define evt ;; FIXME: wrap or handle?
    (wrap-evt (semaphore-peek-evt rsema)
              (lambda (_e) (unbox b))))
  (box-evt b wsema rsema evt))

;; box-evt-set! : BoxEvt X -> Boolean
;; Returns #t if we set, #f if already set.
;; Thread-safe and break-safe but not kill-safe.
(define (box-evt-set! be v)
  (match-define (box-evt b wsema rsema evt) be)
  (define be? (break-enabled))
  (break-enabled #f)
  (cond [(semaphore-try-wait? wsema)
         ;; Consumed wsema; don't post to wsema when done.
         (set-box! b v)
         (semaphore-post rsema)
         (break-enabled be?)
         #t]
        [else
         (break-enabled be?)
         #f]))

(define (box-evt-ready? be)
  (and (sync/timeout (semaphore-peek-evt (box-evt-rsema be))) #t))

;; ----------------------------------------

;; wrap-input-port : InputPort -> (values InputPort (-> Exn Void))
;; FIXME: should reader receive exn immediately or in place of EOF?
(define (wrap-input-port in [name (object-name in)]
                         #:delay-to-eof? [delay-to-eof? #f])
  (define exnbe (make-box-evt))
  (define exn-evt (wrap-evt exnbe raise))
  ;; FIXME: could implement check directly, instead of via sync/timeout
  (define (check) (when (box-evt-ready? exnbe) (sync/timeout 0 exn-evt)))
  (define (check-early) (unless delay-to-eof? (check)))
  (define (check-at-eof) (check))
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
  (values
   (make-input-port* #:name name
                     #:read-in read-in
                     #:peek peek
                     #:close close
                     #:abandon (and (port-with-abandon? in) abandon)
                     #:get-progress-evt get-progress-evt
                     #:commit commit
                     #:get-location get-location
                     #:count-lines! count-lines!)
   (lambda (e) (box-evt-set! exnbe e))))

(define (make-wrapped-pipe)
  (define-values (in out) (make-pipe))
  (define-values (wrapped-in raise-exn) (wrap-input-port in 'wrapped-pipe))
  (values wrapped-in out raise-exn))

;; ----------------------------------------

;; SSL output ports currently seem to create one SSL record per port write.
;; This causes an explosion in the amount of data sent, due to padding, HMAC,
;; and other record overhead. So add an ad hoc buffering port around it.

;; buffering-output-port : OutputPort Boolean [PosInt] -> OutputPort
;; The buffer is unlimited. It is flushed in increments of at most MAX-CHUNK.
;; This wrapper should be thread-safe and break-safe, but it is not kill-safe.
(define (buffering-output-port out propagate-close?
                               [max-flush-chunk-length 4096])
  (define-values (buf-in buf-out) (make-pipe))
  (define flush-lock (make-semaphore 1))
  (define (write-out buf start end non-block? eb?)
    (cond [(< start end)
           (cond [non-block?
                  ;; must not buffer, must not block, eb? = #f
                  (define (retry _) (write-out buf start end #t #f))
                  (cond [(zero? (pipe-content-length buf-in))
                         ;; buffer is empty => write directly to out
                         (write-direct buf start end)]
                        [else ;; buffer is non-empty => try to flush first
                         (cond [(not (semaphore-try-wait? flush-lock))
                                (handle-evt (semaphore-peek-evt flush-lock) retry)]
                               [else ;; acquired flush-lock
                                (if (try-flush-without-block)
                                    (write-direct buf start end)
                                    (begin (semaphore-post flush-lock)
                                           (handle-evt out retry)))])])]
                 [else ;; can buffer, can block
                  buf-out])]
          [else ;; flush => non-block? = #f (allowed to block)
           (begin (flush eb?) 0)]))
  (define (write-direct buf start end) ;; non-block? = #t, eb? = #f
    (or (write-bytes-avail* buf out start end)
        (handle-evt out (lambda (_) (write-out buf start end #t #f)))))
  (define (try-flush-without-block) ;; returns #t if flush completed
    ;; PRE: holding flush-lock, breaks disabled
    (define buffered (pipe-content-length buf-in))
    (cond [(zero? buffered)
           #t]
          [else
           (define len (min buffered (or max-flush-chunk-length buffered)))
           (define progress-evt (port-progress-evt buf-in))
           (define chunk (peek-bytes len buf-in))
           (define r (write-bytes-avail* chunk out))
           (cond [(or (eq? r #f) (zero? r))
                  #f]
                 [else
                  (port-commit-peeked r progress-evt always-evt buf-in)
                  (try-flush-without-block)])]))
  (define (flush/can-block eb?) ;; PRE: holding flush-lock
    (cond [(try-flush-without-block) (void)]
          [else (begin (if eb? (sync/enable-break out) (sync out)) (flush/can-block eb?))]))
  (define (flush eb?)
    (parameterize-break #f
      (if eb? (semaphore-wait/enable-break flush-lock) (semaphore-wait flush-lock))
      (flush/can-block eb?)
      (semaphore-post flush-lock)))
  (define (close)
    (flush #t)
    (when propagate-close? (close-output-port out)))
  (define (abandon)
    (flush #t)
    (when propagate-close? (abandon-port out)))
  (define (get-write-evt buf start end)
    (guard-evt
     (lambda ()
       (let retry ()
         (cond [(not (semaphore-try-wait? flush-lock))
                (replace-evt (semaphore-peek-evt flush-lock)
                             (lambda (_) (retry)))]
               [else ;; acquired flush-lock
                (cond [(parameterize-break #f
                         (begin0 (try-flush-without-block)
                           (semaphore-post flush-lock)))
                       ;; buffer is currently empty
                       (write-bytes-avail-evt buf out start end)]
                      [else (replace-evt out (lambda (_) (retry)))])])))))
  (make-output-port* #:name (object-name out)
                     #:evt always-evt
                     #:write-out write-out
                     #:get-write-evt (and (port-writes-atomic? out)
                                          get-write-evt)
                     #:close close
                     #:abandon (and propagate-close?  ;; otherwise, no point in abandon
                                    (port-with-abandon? out)
                                    abandon)))
