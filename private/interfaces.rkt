;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/class
         racket/match
         racket/struct
         scramble/about)
(provide (all-defined-out))

(define-logger http)
(define-logger http1)
(define-logger http2)

;; ============================================================
;; Who

(define who-mark (gensym 'who))

(define-syntax-rule (with-entry-point who body ...)
  ;; if who is #f, resets to empty
  (with-continuation-mark who-mark who (begin0 (let () body ...) (void))))

(define (http123-who [default-who #f])
  (define eps (continuation-mark-set->list (current-continuation-marks) who-mark))
  (let loop ([eps eps] [default (or default-who 'http123)])
    (cond [(null? eps) default]
          [(eq? (car eps) #f) default]
          [else (loop (cdr eps) (car eps))])))

;; ============================================================

(define-syntax-rule (with-handler handler . body)
  (with-handlers ([(lambda (e) #t) handler]) . body))

;; ============================================================
;; Exceptions

;; Use exn:fail:http123 only for HTTP-related errors.
;; Eg, don't use for malformed header key (but maybe catch and wrap
;; with "during header processing" http123 exn).

;; info : Hash[Symbol => Any], with the following common keys:
;; - 'version  : 'http/1.1 | 'http/2    -- protocol version
;; - 'request  : Request                -- if regarding request
;; - 'received : 'no | 'unknown | 'yes  -- was request processed?
;; - 'code     : Symbol                 -- descriptive symbol
;; - 'where    : (listof Symbol)        -- entrypoint list
;; - 'wrapped-exn : Exn or Any          -- underlying exn
(struct exn:fail:http123 exn:fail (info))

(define h-error-info (make-parameter #hasheq()))

(define (h-error #:info [info #hasheq()]
                 #:wrapped-exn [wrapped-exn #f]
                 #:base-info [base-info (h-error-info)]
                 fmt . args)
  (let* ([info (merge-info base-info info)]
         [info (if wrapped-exn (hash-set info 'wrapped-exn wrapped-exn) info)])
    (let/ec k
      (raise (build-exn (apply format fmt args) info (continuation-marks k))))))

(define (h1-error #:info [info #hasheq()]
                  #:base-info [base-info (h-error-info)]
                  #:wrapped-exn [wrapped-exn #f]
                  fmt . args)
  (apply h-error fmt args
         #:info (hash-set info 'version 'http/1.1)
         #:base-info base-info
         #:wrapped-exn wrapped-exn))

(define (h2-error #:info [info #hasheq()]
                  #:base-info [base-info (h-error-info)]
                  #:wrapped-exn [wrapped-exn #f]
                  fmt . args)
  (apply h-error fmt args
         #:info (hash-set info 'version 'http/2)
         #:base-info base-info
         #:wrapped-exn wrapped-exn))

(define (merge-info base-info info)
  (for/fold ([info info])
            ([(k v) (in-hash base-info)]
             #:when (and v (not (hash-has-key? info k))))
    (hash-set info k v)))

(define (info-details info)
  (define (detail key)
    (cond [(hash-has-key? info key) (format "\n  ~a: ~e" key (hash-ref info key))]
          [else ""]))
  (define (request-detail req)
    (format "\n  request: ~a" (about req)))
  (define (response-detail resp)
    (format "\n  response: ~a" (about resp)))
  (string-append
   (detail 'code)
   (detail 'http2-error)
   ;;(detail 'request)
   (cond [(hash-ref info 'request #f) => request-detail] [else ""])
   (cond [(hash-ref info 'response #f) => response-detail] [else ""])
   (detail 'received)
   (detail 'version)
   (cond [(hash-ref info 'wrapped-exn #f)
          => (lambda (e) (if (exn? e)
                             (format "\n  wrapped error: ~e"
                                     (regexp-replace #rx"\n  .*$" (exn-message e) "..."))
                             ""))]
         [else ""])))

(define (build-exn base-message info [cms (current-continuation-marks)])
  (define who (http123-who (hash-ref info 'who #f)))
  (define details (info-details info))
  (define message (format "~a: ~a~a" who base-message details))
  (exn:fail:http123 message cms info))
