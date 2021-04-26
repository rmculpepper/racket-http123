;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/class
         racket/contract/base
         racket/match
         net/url-structs
         "interfaces.rkt"
         "regexp.rkt"
         "header-base.rkt"
         "header.rkt"
         "request.rkt"
         "response.rkt"
         "connection.rkt"
         "client-base.rkt"
         "util.rkt")
(provide (all-defined-out))

(define default-base-header
  '((#"accept-encoding" #"gzip, deflate")
    (#"user-agent" #"racket-http123/0.1")))

;; ============================================================

(define response/c (is-a?/c http-response<%>))

(define client/c
  (recursive-contract (is-a?/c http-client<%>)))
(define in-header/c
  (listof (or/c string?
                bytes?
                (list/c (or/c symbol? string? bytes?)
                        (or/c string? bytes?)))))
(define status-class/c
  (or/c 'informational 'successful 'redirection 'client-error 'server-error))
(define response-handler-entry/c
  (list/c (or/c (integer-in 100 599) status-class/c)
          (-> client/c response/c any)))
(define content-handler-entry/c
  (list/c (or/c symbol?)
          (-> input-port? any)))

(define http-client<%>
  (interface (http-client-base<%>)
    [fork
     (->*m []
           [#:set-header (or/c #f in-header/c)
            #:add-header in-header/c
            #:set-response-handlers (or/c #f (listof response-handler-entry/c))
            #:add-response-handlers (listof response-handler-entry/c)
            #:set-content-handlers (or/c #f (listof content-handler-entry/c))
            #:add-content-handlers (listof content-handler-entry/c)]
           any)]
    [handle
     (->m request? any)]
    ;; ----
    [adjust-request
     (->m request? request?)]
    [handle-response
     (->m response/c any)]
    [handle-response-content
     (->m response/c any)]
    [sync-request
     (->m request? response/c)]
    ))

;; ============================================================

(define current-response (make-parameter #f))

(define http-client%
  (class* object% (http-client<%>)
    (init-field [base (new connection-manager%)]
                [base-header default-base-header]
                [response-handlers null]
                [content-handlers null])
    (super-new)

    (define/public (fork #:set-header [set-header #f]
                         #:add-header [in-add-header null]
                         #:set-response-handlers [set-response-handlers #f]
                         #:add-response-handlers [new-response-handlers null]
                         #:set-content-handlers [set-content-handlers #f]
                         #:add-content-handlers [new-content-handlers null])
      (with-entry-point 'fork
        (define add-header (check-header-field-list in-add-header))
        (new http-client%
             (base base)
             (base-header
              (let ([base-header (if set-header (check-header-field-list set-header) base-header)])
                (header-field-list-update base-header add-header)))
             (response-handlers
              (append (or set-response-handlers response-handlers) new-response-handlers))
             (content-handlers
              (append (or set-content-handlers content-handlers) new-content-handlers)))))

    (define/public (adjust-request req)
      (match-define (request method url header data) req)
      (define new-header (header-field-list-update base-header header))
      (request method url new-header data))

    (define/public (handle req)
      (with-entry-point 'handle
        (handle-response (sync-request req))))

    (define/public (handle-response resp)
      (define handler
        (cond [(assoc* (list (send resp get-status-code)
                             (send resp get-status-class)
                             'else)
                       response-handlers)
               => cadr]
              [else #f]))
      (if handler
          (handler this resp)
          (default-response-handler resp)))

    (define/public (default-response-handler resp)
      (case (send resp get-status-code)
        [(200) (handle-response-content resp)]
        [else (unhandled-response resp)]))

    (define/public (unhandled-response resp)
      (send resp close-content-in)
      (h-error "no response handler matched"
               (send resp get-status-class)
               #:info (hasheq 'code 'unhandled-response
                              'received 'yes
                              'response resp)))

    (define/public (handle-response-content resp)
      (define type (or (send resp get-content-type) #f))
      (define handler
        (cond [(assoc* (list type '*/*) content-handlers) => cadr]
              [else #f]))
      (if handler
          (parameterize ((current-response resp))
            (handler (send resp get-content-in #t)))
          (default-content-handler resp)))

    (define/public (default-content-handler resp)
      (unhandled-content resp))

    (define/public (unhandled-content resp)
      (send resp close-content-in)
      (h-error "no content handler matched"
               #:info (hasheq 'code 'unhandled-content
                              'received 'yes
                              'response resp)))

    ;; ----------------------------------------

    (define/public (async-request req)
      (with-entry-point 'async-request
        (send base async-request (adjust-request req))))

    (define/public (sync-request req)
      (with-entry-point 'sync-request
        ((sync (async-request req)))))
    ))

;; ----------------------------------------

;; Goal for merging:
;;   (hash-ref merged key #f) = (or (hash-ref new key #f) (hash-ref old key #f))

;; FIXME: check hash contents

(define (assoc* ks alist)
  (match alist
    ['() #f]
    [(cons (and entry (cons k _)) alist)
     (if (member k ks) entry (assoc* ks alist))]))

;; ----------------------------------------

#;
(begin (define req (request 'GET "https://www.google.com/"))
       (define req1 (request 'GET "http://www.neverssl.com/"))
       (define c0 (new http-client%))
       (require racket/port)
       (define c (send c0 fork #:add-content-handlers `([text/html ,port->string]))))

;; FIXME: beware empty paths: (GET "https://www.google.com") fails!
