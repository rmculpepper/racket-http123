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

(define http-client<%>
  (interface (http-client-base<%>)
    [sync-request
     (->m request? response/c)]
    ))

;; ============================================================

(define current-response (make-parameter #f))

(define http-client%
  (class* object% (http-client<%>)
    (init-field [base (new connection-manager%)]
                [base-header default-base-header]
                [response-handlers (hash)]
                [content-handlers (hash)])
    (super-new)

    (define/public (fork #:add-header [in-add-header null]
                         #:add-response-handlers [new-response-handlers (hash)]
                         #:add-content-handlers [new-content-handlers (hash)])
      (with-entry-point 'fork
        (define add-header (check-header-field-list in-add-header))
        (new http-client%
             (base base)
             (base-header (header-field-list-update base-header add-header))
             (response-handlers (merge-response-handlers response-handlers new-response-handlers))
             (content-handlers (merge-content-handlers content-handlers new-content-handlers)))))

    (define/public (adjust-request req)
      (match-define (request method url header data) req)
      (define new-header (header-field-list-update base-header header))
      (request method url new-header data))

    (define/public (handle req)
      (with-entry-point 'handle
        (handle-response (sync-request req))))

    (define/public (handle-response resp)
      (define handler
        (or (hash-ref response-handlers (send resp get-status-code) #f)
            (hash-ref response-handlers (send resp get-status-class) #f)
            (hash-ref response-handlers 'else #f)))
      (if handler
          (handler this resp)
          (default-handler resp)))

    (define/public (default-handler resp)
      (case (send resp get-status-code)
        [(200) (handle-response-content resp)]
        [else (unhandled-response resp)]))

    (define/public (unhandled-response resp)
      (h-error "no handler matched for ~a response"
               (send resp get-status-class)
               #:info (hasheq 'code 'unhandled-response
                              'received 'yes
                              'response resp)))

    (define/public (handle-response-content resp)
      (define h (send resp get-header))
      (define content-type (or (send h get-value 'content-type) #""))
      (match (regexp-match (rx (rx ^ (record TOKEN) "/" (record TOKEN))) content-type)
        [(list _ type-bs subtype-bs)
         (define type (string->symbol (format "~a/~a" type-bs subtype-bs)))
         (define handler
           (or (hash-ref content-handlers type #f)
               (hash-ref content-handlers '*/* #f)))
         (if handler
             (parameterize ((current-response resp))
               (handler (send resp get-content-in #t)))
             (default-content-handler resp type))]
        [_ (default-content-handler resp #f)]))

    (define/public (default-content-handler resp type)
      (h-error "no content handler matched for ~a content-type" (or type "unknown")
               #:info (hasheq 'code 'unhandled-response-content
                              'content-type type
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

(define (merge-response-handlers old new)
  (for/fold ([h new]) ([(k v) (in-hash old)])
    (if (or (hash-has-key? new k)
            (hash-has-key? new (status-code->class k))
            (hash-has-key? new 'else))
        h
        (hash-set h k v))))

(define (merge-content-handlers old new)
  (if (hash-has-key? new '*/*)
      new
      (for/fold ([h old]) ([(k v) (in-hash new)])
        (hash-set h k v))))

;; ----------------------------------------

#;
(begin (define req (request 'GET "https://www.google.com/"))
       (define req1 (request 'GET "http://www.neverssl.com/"))
       (define c0 (new http-client%))
       (require racket/port)
       (define c (send c0 fork #:add-content-handlers (hasheq 'text/html port->string))))

;; FIXME: beware empty paths: (GET "https://www.google.com") fails!
