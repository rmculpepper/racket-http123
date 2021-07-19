;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/class
         racket/contract/base
         racket/match
         file/gunzip
         scramble/class
         "interfaces.rkt"
         "request.rkt"
         (submod "util.rkt" port)
         "header.rkt")
(provide (all-defined-out))

;; FIXME/TODO:
;; - record headers actually sent, including UA-synthesized (eg, Host)

(define response<%>
  (interface (about<%>)
    [get-version
     (->m symbol?)]
    [get-request
     (->m (or/c request? #f))]
    [get-status-code
     (->m exact-nonnegative-integer?)]
    [get-status-class
     (->m symbol?)]
    [get-header
     (->m (is-a?/c header<%>))]
    [get-content-type
     (->m (or/c #f symbol?))]
    [get-content-in
     (->*m [] [boolean?] (or/c #f input-port?))]
    [close-content-in
     (->m void?)]
    [get-trailer
     (->m (or/c #f (is-a?/c header<%>)))]
    [get-trailer-evt
     (->m (evt/c (-> (or/c #f (is-a?/c header<%>)))))]
    [about
     (->m string?)]
    [aux-info
     (case->m
      (-> (and/c hash? hash-eq? immutable?))
      (-> (and/c hash? hash-eq? immutable?) void?))]
    ))

;; ------------------------------------------------------------

(define http-response%
  (class* object% (response<%> constructor-style-printable<%>)
    (init-field request         ;; Request
                status-code     ;; Nat
                header          ;; header%
                trailerbxe)     ;; (Evt (-> (or/c #f header%)))
    (init ((init-content content))) ;; #f or Bytes or InputPort
    (field [aux #hasheq()])      ;; Hasheq[Any => Any]
    (super-new)

    ;; content-in : #f or InputPort
    (field [content-in (cond [(bytes? init-content) (open-input-bytes init-content)]
                             [(input-port? init-content) init-content]
                             [else #f])])

    (define/public (get-request) request)
    (define/public (get-status-code) status-code)
    (define/public (get-status-class)
      (status-code->class status-code))
    (define/public (get-header) header)
    (define/public (has-content?) (and content-in #t))
    (define/public (get-content-in [empty-port? #f])
      (or content-in (if empty-port? (open-input-bytes #"") #f)))
    (define/public (get-content-type)
      (send header get-content-type))
    (define/public (close-content-in)
      (when content-in (close-input-port content-in)))

    (abstract get-version)

    (define/public (get-trailer-evt)
      (or trailerbxe const-false-evt))
    (define/public (get-trailer)
      (and trailerbxe ((sync trailerbxe))))

    (define/public aux-info
      (case-lambda
        [() aux]
        [(new-aux) (set! aux new-aux)]))

    ;; ----

    (define/public (get-printing-class-name)
      'http-response%)
    (define/public (get-printing-components)
      (values '(status-code request header) (list status-code request header) #t))
    (define/public (about)
      (format "~a response with ~a body"
              status-code
              (if content-in (get-content-type) "no")))
    ))

(define const-false-evt
  (let ([const-false (lambda () #f)])
    (wrap-evt always-evt (lambda (ae) const-false))))

;; ----------------------------------------

(define decoding-response%
  (class http-response%
    (init header
          content
          [handle-content-encoding? #t])
    (let ()
      (define decode-mode (get-decode-mode header))
      (define content*
        (case (and content decode-mode)
          [(gzip deflate)
           (let ([content-in (if (bytes? content) (open-input-bytes content) content)])
             (make-decode-input-wrapper decode-mode content-in))]
          [else content]))
      (super-new (header header) (content content*)))
    ))

;; get-decode-mode : Header -> (U 'gzip 'deflate #f)
(define (get-decode-mode header)
  (cond [(send header has-value? #"content-encoding" #"gzip") 'gzip]
        [(send header has-value? #"content-encoding" #"deflate") 'deflate]
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

;; ----------------------------------------

(define http11-response%
  (class* decoding-response% ()
    (init-field [status-line #f])
    (super-new)

    (define/override (get-version) 'http/1.1)
    (define/public (get-status-line) status-line)
    (define/override (get-printing-class-name) 'http11-response%)
    ))

(define http2-response%
  (class* decoding-response% ()
    (super-new)
    (define/override (get-version) 'http/2)
    (define/override (get-printing-class-name) 'http2-response%)
    ))

;; ============================================================
;; Status Codes

;; Reference:
;; - https://tools.ietf.org/html/rfc7231#section-6

(define (status-code->class status-code)
  (cond [(<= 100 status-code 199) 'informational]
        [(<= 200 status-code 299) 'successful]
        [(<= 300 status-code 399) 'redirection]
        [(<= 400 status-code 499) 'client-error]
        [(<= 500 status-code 599) 'server-error]))

(define (status-code-cacheable-by-default? status-code) ;; 6.1
  (and (memq status-code '(200 203 204 206 300 301 404 405 410 414 501)) #t))

(define (status-code-with-content? status-code) ;; 3.3.3
  ;; #t means might have content; #f means certainly does not
  ;; Also, response to HEAD request.
  (cond [(<= 100 status-code 199) #f]
        [(memv status-code '(204 304)) #f]
        [else #t]))

(define (status-code-reason code)
  (case code
    [(100) "Continue"]
    [(101) "Switching Protocols"]
    [(200) "OK"]
    [(201) "Created"]
    [(202) "Accepted"]
    [(203) "Non-Authoritative Information"]
    [(204) "No Content"]
    [(205) "Reset Content"]
    [(206) "Partial Content"]
    [(300) "Multiple Choices"]
    [(301) "Moved Permanently"]
    [(302) "Found"]
    [(303) "See Other"]
    [(304) "Not Modified"]
    [(305) "Use Proxy"]
    [(307) "Temporary Redirect"]
    [(400) "Bad Request"]
    [(401) "Unauthorized"]
    [(402) "Payment Required"]
    [(403) "Forbidden"]
    [(404) "Not Found"]
    [(405) "Method Not Allowed"]
    [(406) "Not Acceptable"]
    [(407) "Proxy Authentication Required"]
    [(408) "Request Timeout"]
    [(409) "Conflict"]
    [(410) "Gone"]
    [(411) "Length Required"]
    [(412) "Precondition Failed"]
    [(413) "Payload Too Large"]
    [(414) "URI Too Long"]
    [(415) "Unsupported Media Type"]
    [(416) "Range Not Satisfiable"]
    [(417) "Expectation Failed"]
    [(426) "Upgrade Required"]
    [(500) "Internal Server Error"]
    [(501) "Not Implemented"]
    [(502) "Bad Gateway"]
    [(503) "Service Unavailable"]
    [(504) "Gateway Timeout"]
    [(505) "HTTP Version Not Supported"]
    [else #f]))
