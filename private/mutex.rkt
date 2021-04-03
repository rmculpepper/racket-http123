#lang racket/base

(struct mutex (owner-box sema peek))

;; sema=1 owner=never-evt     -- unlocked
;; sema=1 owner=<thd-dead>    -- acquired, gate not up yet
;; sema=0 owner=<thd-dead>    -- acquired, gate up
;; sema=0 owner=never-evt     -- NOT ALLOWED

(define (make-mutex)
  (define sema (make-semaphore 1))
  (mutex (owner never-evt) sema (semaphore-peek-evt sema)))

(define (mutex-acquire m)
  (case (mutex-acquire* m)
    [(acquired)
     #t]
    [(hopeless)
     (error 'mutex-acquire "mutex locked by a dead thread")]
    [(recursive)
     (error 'mutex-acquire "attempted to recursively acquire mutex")]))

(define (mutex-acquire* m)
  (match-define (mutex owner-box sema peek) m)
  (define me (thread-dead-evt (current-thread)))
  (cond [(eq? me (unbox owner-box))
         'recursive]
        [else
         (let loop ()
           (define v (sync (unbox owner-box) peek))
           (cond [(eq? v peek)
                  ;; Got in the outer gate...
                  (cond [(box-cas! owner-box never-evt me)
                         ;; acquired mutex
                         (semaphore-wait sema)
                         'acquired]
                        [else
                         ;; lost race for box => try again
                         (loop)])]
                 ;; Otherwise, got a thread-dead evt, which means the
                 ;; current *or previous* owner is dead.
                 [(eq? (unbox owner-box) v)
                  ;; v is not stale => owner is actually dead.
                  'hopeless]
                 [else
                  ;; v was stale => try again
                  (loop)]))]))

(define (mutex-release m)
  (match-define (mutex owner-box sema peek) m)
  (semaphore-post sema)
  (set-box! owner-box never-evt)
  (void))
