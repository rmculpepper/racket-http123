#lang racket/base
(require racket/class
         racket/match
         racket/struct)
(provide (all-defined-out))

(define-logger http)
(define-logger http1)
(define-logger http2)

(define default-user-agent "racket-http123/1.0")

(define default-accept-encoding #"gzip, deflate")

;; ============================================================
;; Interfaces

;; ------------------------------------------------------------
;; HTTP Client

;; http<%> represents the public interface of an http client
(define http<%>
  (interface ()
    ;; ----------------------------------------
    ;; Convenience methods
    GET
    POST
    HEAD
    PUT
    DELETE

    OPTIONS
    TRACE
    CONNECT
    ))

;; ------------------------------------------------------------
;; Response

;; ============================================================
;; Who

(define who-mark (gensym 'who))

(define-syntax-rule (with-entry-point who body ...)
  ;; if who is #f, resets to empty
  (with-continuation-mark who-mark who (begin0 (let () body ...))))

(define (get-entry-point default-who)
  (define eps (continuation-mark-set->list (current-continuation-marks) who-mark))
  (let loop ([eps eps] [default (or default-who 'http123)])
    (cond [(null? eps) default]
          [(eq? (car eps) #f) default]
          [else (loop (cdr eps) (car eps))])))

(define (http123-who [default-who 'http123]) (get-entry-point default-who))

(define h-error-info (make-parameter #hasheq()))

(define (error* #:who [who #f]
                #:code [code #f]
                fmt . args)
  (define info (h-error-info))
  (if (hash-empty? info)
      (apply error (http123-who who) fmt args)
      (apply h-error #:who who #:code code #:info info fmt args)))

(define-syntax-rule (with-handler handler . body)
  (with-handlers ([(lambda (e) #t) handler]) . body))

(define (make-handler message-prefix base-info)
  (lambda (e) (merge-exn base-info e)))

(define (merge-exn e message-prefix base-info)
  (define-values (msg cms info)
    (match e
      [(exn:fail:http123 msg cms info)
       (values msg cms info)]
      [(exn msg cms) (values msg cms #hasheq())]
      [v (values (format "non-exception value raised: ~e" v) #f #hasheq())]))
  (exn:fail:http123 (if message-prefix
                        (format "~a: ~a;\n ~a"
                                (http123-who (hash-ref base-info 'who #f))
                                message-prefix msg)
                        msg)
                    (or cms (current-continuation-marks))
                    (hash-set (merge-info base-info info)
                              'wrapped-exn e)))

(define (merge-info base-info info)
  (for/fold ([info info])
            ([(k v) (in-hash base-info)]
             #:when (and v (not (hash-has-key? info k))))
    (hash-set info k v)))

(define (build-exn base-message info [cms (current-continuation-marks)])
  (define who (http123-who (hash-ref info 'who #f)))
  (define details (info-details info))
  (define message (format "~a: ~a~a" who base-message details))
  (exn:fail:http123 message cms info))

(define (info-details info)
  (define (detail key)
    (cond [(hash-has-key? info key) (format "\n  ~a: ~e" key (hash-ref info key))]
          [else ""]))
  (define (request-detail req)
    (format "\n  request: ~a" ((about-ref req) req)))
  (string-append
   (detail 'code)
   (detail 'http2-error)
   ;;(detail 'request)
   (cond [(hash-ref info 'request #f) => request-detail] [else ""])
   (detail 'received)
   (detail 'version)
   (cond [(hash-ref info 'wrapped-exn #f)
          => (lambda (e) (if (exn? e)
                             (format "\n  wrapped error: ~e" (exn-message e))
                             ""))]
         [else ""])))

(define-values (prop:about aboutable? about-ref)
  (make-struct-type-property 'about))

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

(define (h-error #:info [info #hasheq()]
                 #:wrapped-exn [wrapped-exn #f]
                 #:base-info [base-info (h-error-info)]
                 fmt . args)
  (let* ([info (merge-info base-info info)]
         [info (if wrapped-exn (hash-set info 'wrapped-exn wrapped-exn) info)])
    (let/ec k
      (raise (exn:fail:http123
              (format "~a: ~a"
                      (http123-who (hash-ref info 'who #f))
                      (apply format fmt args))
              (continuation-marks k)
              info)))))

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

;; FIXME: Goal: should be clear to user whether request was sent and
;; received (as much as we can know, anyway)
;; - If not exn:fail:http123 (eg, exn:fail:contract, plain exn:fail),
;;   then request was not sent.
;; - If http:fail:http123, then consult 'received field.
;;   That may require catching underlying exn; if so, add as info 'underlying-exn.

;; ============================================================
;; Printing

(struct print:init (name value)
  #:property prop:custom-write
  (make-constructor-style-printer
   (lambda (self) (print:init-name self))
   (lambda (self) (list (print:init-value self)))))

(define class-printable<%>
  (interface* () ([prop:custom-write
                   (let ()
                     (define ((emit-fields make-init) self)
                       (define-values (fieldnames fieldvals more?)
                         (send self get-printing-components))
                       (append (for/list ([fieldname (in-list fieldnames)]
                                          [fieldval (in-list fieldvals)])
                                 (make-init fieldname fieldval))
                               (if more? (list (unquoted-printing-string "...")) '())))
                     (define (emit-new-classname self)
                       (string->symbol (format "new ~a" (send self get-printing-classname))))
                     (define (emit-classname self)
                       (string->symbol (format "~a" (send self get-printing-classname))))
                     (define writer
                       (make-constructor-style-printer emit-classname (emit-fields list)))
                     (define printer
                       (make-constructor-style-printer emit-new-classname (emit-fields print:init)))
                     (lambda (self out mode)
                       (case mode
                         [(#t #f) (writer self out mode)]
                         [else (printer self out mode)])))])
    get-printing-classname
    get-printing-components))

(define class-never-quotable<%>
  (interface* () ([prop:custom-print-quotable 'never])))
