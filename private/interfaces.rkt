#lang racket/base
(require racket/class)
(provide (all-defined-out))

(define-logger http)

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

(define (http123-entry-points) ;; reversed, ie earliest->latest
  (define entry-points
    (continuation-mark-set->list (current-continuation-marks) who-mark))
  (if (pair? entry-points) (reverse entry-points) '(http123)))

(define (http123-who)
  (car (http123-entry-points)))

(define (error* fmt . args)
  (apply error (http123-who) fmt args))

(define (internal-error fmt . args)
  (apply error (http123-who) (string-append "internal error: " fmt) args))

;; ============================================================
;; Exceptions

;; exn:fail:http123 indicates an error that occurred
;; where party : #f or 'server or 'client
;;       code  : Symbol or #f
;;       info  : (Listof (cons Symbol Any))
(struct exn:fail:http123 exn:fail (party code info))
(struct exn:fail:http123:client exn:fail:http123 ())
(struct exn:fail:http123:server exn:fail:http123 ())

(define (http-error #:who [who #f] #:party [party #f] #:code [code #f] #:info [info null]
                    fmt . args)
  (let/ec k
    (raise (exn:fail:http123
            (format "~a: ~a" (or who (http123-who)) (apply format fmt args))
            (continuation-marks k)
            party code info))))

(define (c-error #:who [who #f] #:code [code #f] #:info [info null] fmt . args)
  (apply http-error #:who who #:party 'client #:info info fmt args))
(define (s-error #:who [who #f] #:code [code #f] #:info [info null] fmt . args)
  (apply http-error #:who who #:party 'server #:info info fmt args))
