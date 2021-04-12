#lang racket/base
(require racket/class
         "io.rkt"
         file/gunzip)
(provide (all-defined-out))

;; FIXME: move to response layer

;; get-decode-mode : Headers -> (U 'gzip 'deflate #f)
(define (get-decode-mode headers)
  (cond [(send headers has-value? 'content-encoding #"gzip") 'gzip]
        [(send headers has-value? 'content-encoding #"deflate") 'deflate]
        [else #f]))

(define (make-decode-input-wrapper decode-mode decode-in)
  (cond [(memq decode-mode '(gzip deflate))
         (define-values (user-in out-to-user raise-user-exn) (make-wrapped-pipe))
         (thread (lambda ()
                   (with-handlers ([exn? (lambda (e) (raise-user-exn e))])
                     (case decode-mode
                       [(gzip) (gunzip-through-ports decode-in out-to-user)]
                       [(deflate) (inflate decode-in out-to-user)]))
                   (close-output-port out-to-user)))
         user-in]
        [else decode-in]))
