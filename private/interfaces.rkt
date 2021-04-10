#lang racket/base
(require racket/class
         racket/match
         racket/struct)
(provide (all-defined-out))

(define-logger http)
(define-logger http1)
(define-logger http2)

(define default-user-agent
  (format "Racket/~a (http123)" (version)))

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

;; implementation interface
(define http-impl<%>
  (interface ()
    connect ;; String PortNat -> http-connection<%> or error
    ;; Connect to host:port (or return existing connection).

    disconnect ;; String PortNat -> Void
    ;; Advice to drop existing connection to host:port (omit?)
    ))

;; ------------------------------------------------------------

;; http-connection<%> represents a connection to an HTTP server
(define http-connection<%>
  (interface ()

    request/method ;;
    ))

;; ------------------------------------------------------------
;; Response

(define http-response<%>
  (interface ()
    get-response-code ;; -> Nat, eg 200
    get-response-line ;; -> Bytes
    get-headers ;; -> ??
    get-header ;; -> ??
    get-content ;; -> Any -- usually Bytes
    ))

;; ------------------------------------------------------------
;; Cookie Jar

(define cookie-jar<%>
  (interface ()
    ))

;; ============================================================
;; Who

(define who-mark (gensym 'who))

(define-syntax-rule (with-entry-point who body ...)
  (with-continuation-mark who-mark who (begin0 (let () body ...))))

(define (get-entry-points) ;; reversed, ie earliest->latest
  (define entry-points
    (continuation-mark-set->list (current-continuation-marks) who-mark))
  (if (pair? entry-points) (reverse entry-points) '(http123)))

(define (http123-who)
  (car (get-entry-points)))

(define h-error-info (make-parameter #hasheq()))

(define (error* #:who [who #f]
                #:code [code #f]
                fmt . args)
  (define info (h-error-info))
  (cond [(not (hash-empty? info))
         => (lambda (info)
              (apply h-error #:who who #:code code #:info info fmt args))]
        [else (apply error (or (http123-who) who 'http123) fmt args)]))

(define (internal-error fmt . args)
  (apply error (http123-who) (string-append "internal error: " fmt) args))

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
  (raise (exn:fail:http123 (if message-prefix
                               (string-append message-prefix ";\n " msg)
                               msg)
                           (or cms (current-continuation-marks))
                           (hash-set (merge-info base-info info)
                                     'wrapped-exn e))))

(define (merge-info base-info info)
  (for/fold ([info info])
            ([(k v) (in-hash base-info)]
             #:when (and v (not (hash-has-key? info k))))
    (hash-set info k v)))

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

(define (h-error #:party [party #f]
                 #:code [code #f]
                 #:received [received #f]
                 #:wrapped-exn [wrapped-exn #f]
                 #:version [version #f]
                 #:info [base-info #hasheq()]
                 #:who [who 'http123]
                 fmt . args)
  (let/ec k
    (raise (exn:fail:http123
            (format "~a: ~a" (or (http123-who) who) (apply format fmt args))
            (continuation-marks k)
            (hash-set** base-info
                        '(party code received wrapped-exn version who)
                        (list party code received wrapped-exn version who))))))

(define (hash-set** h ks vs)
  (let loop ([h h] [ks ks] [vs vs])
    (cond [(pair? ks)
           (let ([h (if (car vs) (hash-set h (car ks) (car vs)) h)])
             (loop h (cdr ks) (cdr vs)))]
          [else h])))

(define (h2-error #:party [party #f]
                  #:code [code #f]
                  #:received [received #f]
                  #:wrapped-exn [wrapped-exn #f]
                  #:info [base-info #hasheq()]
                  #:who [who 'http123]
                  fmt . args)
  (apply h-error fmt args
         #:party party #:code code #:received received #:wrapped-exn wrapped-exn
         #:info base-info #:who who #:version 'http/2))


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
