;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/class
         racket/contract/base
         racket/match
         net/url-structs
         net/cookies/user-agent
         "interfaces.rkt"
         "regexp.rkt"
         "header-base.rkt"
         "header.rkt"
         "request.rkt"
         "response.rkt"
         "connection.rkt"
         (submod "util.rkt" url))
(provide (all-defined-out))

(define default-base-header
  '((#"accept-encoding" #"gzip, deflate")
    (#"user-agent" #"racket-http123/0.1")))

(define REDIRECT-LIMIT 5)

;; ============================================================

(define response/c (is-a?/c response<%>))

(define client/c
  (recursive-contract (is-a?/c http-client<%>)))
(define in-header/c
  (listof (or/c string?
                bytes?
                (list/c (or/c symbol? string? bytes?)
                        (or/c string? bytes?)))))
(define status-class/c
  (or/c 'informational 'successful 'redirection 'client-error 'server-error))
(define response-handler/c
  (-> client/c response/c any))
(define response-handler-entry/c
  (list/c (or/c (integer-in 100 599) status-class/c)
          response-handler/c))
(define content-handler/c
  (-> input-port? any))
(define content-handler-entry/c
  (list/c (or/c symbol?) content-handler/c))

(define http-client<%>
  (interface (http-client-base<%>)
    [fork
     (->*m []
           [#:add-header in-header/c
            #:add-response-handlers (listof response-handler-entry/c)
            #:add-content-handlers (listof content-handler-entry/c)
            #:add-request-adjuster (or/c #f (-> request? request?))
            #:add-response-listener (or/c #f (-> response/c void?))
            #:add-cookie-jar (or/c #f (is-a?/c cookie-jar<%>))]
           any)]
    [handle
     (->*m [request?]
           [#:aux-info aux-info/c]
           any)]
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
    (init [ssl 'secure])
    (init-field [base (new connection-manager% (ssl ssl))]
                [base-header default-base-header]
                [response-handlers null]
                [content-handlers null]
                [request-adjusters null]
                [response-listeners null])
    (super-new)

    (define/public (fork #:add-header [in-add-header null]
                         #:add-response-handlers [new-response-handlers null]
                         #:add-content-handlers [new-content-handlers null]
                         #:add-request-adjuster [new-request-adjuster #f]
                         #:add-response-listener [new-resp-listener #f]
                         #:add-cookie-jar [cookie-jar #f])
      (define (list-if x) (if x (list x) null))
      (define-values (cookie-adjuster cookie-listener)
        (if cookie-jar
            (let ([lock (make-semaphore 1)])
              (values (make-cookie-request-adjuster cookie-jar lock)
                      (make-cookie-response-listener cookie-jar lock)))
            (values #f #f)))
      (with-entry-point 'fork
        (define add-header (check-header-field-list in-add-header))
        (new http-client%
             (base base)
             (base-header (header-field-list-update base-header add-header))
             (response-handlers (append new-response-handlers response-handlers))
             (content-handlers (append new-content-handlers content-handlers))
             (request-adjusters
              (append (list-if new-request-adjuster) (list-if cookie-adjuster) request-adjusters))
             (response-listeners
              (append response-listeners (list-if cookie-listener) (list-if new-resp-listener))))))

    (define/public (adjust-request req)
      (with-entry-point 'adjust-request
        (match-define (request method url header data) req)
        (define new-header (header-field-list-update base-header header))
        (for/fold ([req (request method url new-header data)])
                  ([adj (in-list request-adjusters)])
          (adj req))))

    (define/public (handle req #:aux-info [aux '#hasheq()])
      (with-entry-point 'handle
        (define resp (sync-request req))
        (send resp aux-info aux)
        (handle-response (sync-request req))))

    (define/public (handle-response resp)
      (with-entry-point 'handle-response
        (for ([listener (in-list response-listeners)])
          (listener resp))
        (define handler
          (cond [(assoc* (list (send resp get-status-code)
                               (send resp get-status-class)
                               'else)
                         response-handlers)
                 => cadr]
                [else #f]))
        (if handler
            (handler this resp)
            (default-response-handler resp))))

    (define/public (default-response-handler resp)
      (with-entry-point 'default-response-handler
        (case (send resp get-status-code)
          [(200) (handle-response-content resp)]
          [else (unhandled-response resp)])))

    (define/private (unhandled-response resp)
      (send resp close-content-in)
      (h-error "no response handler matched"
               #:info (hasheq 'code 'unhandled-response
                              'received 'yes
                              'response resp)))

    (define/public (handle-response-content resp)
      (with-entry-point 'handle-response-content
        (define type (or (send resp get-content-type) #f))
        (define handler
          (cond [(assoc* (list type '*/*) content-handlers) => cadr]
                [else #f]))
        (if handler
            (parameterize ((current-response resp))
              (handler (send resp get-content-in #t)))
            (default-response-content-handler resp))))

    (define/public (default-response-content-handler resp)
      (with-entry-point 'default-response-content-handler
        (unhandled-content resp)))

    (define/private (unhandled-content resp)
      (send resp close-content-in)
      (h-error "no content handler matched"
               #:info (hasheq 'code 'unhandled-content
                              'received 'yes
                              'response resp)))

    ;; ----------------------------------------

    ;; FIXME: MUST NOT reuse request header for different domain
    ;; without more consideration. Maybe reset to null, maybe add
    ;; opt-in list of fields to copy?

    (define/public (handle-redirection resp
                                       #:limit [limit REDIRECT-LIMIT]
                                       #:redirect-type [redirect-type #f]
                                       #:fail [fail (lambda () (unhandled-redirection resp))])
      (with-entry-point 'handle-redirection
        ;; Note: Don't reuse request header for different domain!
        (match-define (request req-method req-url _ req-data) (send resp get-request))
        (send resp close-content-in)
        (define h (send resp get-header))
        (define aux (send resp aux-info))
        (define redirected-from
          (let ([rs (hash-ref aux '_redirected-from null)])
            (cons resp (if (list? rs) rs (list rs)))))
        (define new-aux (hash-set aux '_redirected-from redirected-from))
        (define loc (send h get-ascii-value #"location"))
        (define new-req
          (cond [(and loc (<= (length redirected-from) limit))
                 (define u (effective-url req-url loc))
                 (case (or redirect-type
                           (status-code->redirection-type (send resp get-status-code)))
                   [(POST->GET) ;; if POST, change to GET and drop data; else, unchanged
                    (case req-method
                      [(POST) (request 'GET u null #f)]
                      [else (request req-method u null req-data)])]
                   [(all->GET) ;; change method to GET, always (except HEAD?)
                    (define new-method (case req-method [(HEAD) 'HEAD] [else 'GET]))
                    (request new-method u null #f)]
                   [(same-method) ;; must keep original method
                    (request req-method u null req-data)]
                   [else #f])]
                [else #f]))
        (cond [new-req (handle new-req #:aux-info new-aux)]
              [else (fail)])))

    (define/private (effective-url base-url loc)
      ;; FIXME: resolve relative to base-url
      (check-http-url 'handle-redirection loc))

    (define/public (status-code->redirection-type code)
      (case code
        [(301 302) 'POST->GET]
        [(303) 'all->GET]
        [(307 308) 'same-method]
        [else #f]))

    (define/private (unhandled-redirection resp)
      (h-error "unable to automatically handle redirection"
               #:info (hasheq 'code 'unhandled-redirection
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

(define (assoc* ks alist)
  (match alist
    ['() #f]
    [(cons (and entry (cons k _)) alist)
     (if (member k ks) entry (assoc* ks alist))]))

;; ----------------------------------------

(define ((make-cookie-request-adjuster cookie-jar lock) req)
  (match-define (request method url header data) req)
  (define cookies
    (call-with-semaphore lock (lambda () (send cookie-jar cookies-matching url))))
  (define encode string->bytes/utf-8)
  (define cookie-header-fields
    (for/list ([cookie (in-list cookies)])
      (list #"cookie" (bytes-append (encode (ua-cookie-name cookie))
                                    #"="
                                    (encode (ua-cookie-value cookie))))))
  (request method url (append header cookie-header-fields) data))

(define ((make-cookie-response-listener cookie-jar lock) resp)
  (define header (send resp get-header))
  (define cookies (send header get-values 'set-cookie))
  (when cookies
    (define c-header (for/list ([cookie (in-list cookies)]) (cons #"set-cookie" cookie)))
    (define url (request-url (send resp get-request)))
    (parameterize ((current-cookie-jar cookie-jar))
      (call-with-semaphore lock (lambda () (extract-and-save-cookies! c-header url)))))
  (void))

;; ----------------------------------------

#;
(begin (define req (request 'GET "https://www.google.com/"))
       (define req1 (request 'GET "http://www.neverssl.com/"))
       (define c0 (http-client))
       (require racket/port net/cookies/user-agent)
       (define c (send c0 fork #:add-content-handlers `([text/html ,port->string])
                       #:add-cookie-jar (current-cookie-jar))))
