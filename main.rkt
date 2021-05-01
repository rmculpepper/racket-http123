;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/class
         racket/contract/base
         openssl
         net/cookies/user-agent
         "private/interfaces.rkt"
         "private/request.rkt"
         "private/header-base.rkt"
         "private/header.rkt"
         "private/response.rkt"
         "private/client-base.rkt"
         "private/client.rkt"
         "private/util.rkt")
(provide (struct-out exn:fail:http123)
         request
         request?
         request-method
         request-url
         request-header
         request-data
         ok-http-url?
         response<%>
         header<%>
         header-key-symbol?
         http-client-base<%>
         http-client<%>
         current-response

         status-class/c
         response-handler/c
         content-handler/c

         (contract-out
          [http-client
           (->* []
                [#:ssl (or/c 'secure 'auto ssl-client-context?)
                 #:add-response-handlers (listof response-handler-entry/c)
                 #:add-content-handlers (listof content-handler-entry/c)
                 #:add-request-adjuster (or/c #f (-> request? request?))
                 #:add-response-listener (or/c #f (-> response/c void?))
                 #:add-cookie-jar (or/c #f (is-a?/c cookie-jar<%>))]
                (is-a?/c http-client<%>))]))

(define (http-client #:ssl [ssl 'secure]
                     #:add-header [new-header null]
                     #:add-response-handlers [new-response-handlers null]
                     #:add-content-handlers [new-content-handlers null]
                     #:add-request-adjuster [request-adjuster #f]
                     #:add-response-listener [response-listener #f]
                     #:add-cookie-jar [cookie-jar #f])
  (define c (new http-client% (ssl ssl)))
  (send c fork
        #:add-header new-header
        #:add-response-handlers new-response-handlers
        #:add-content-handlers new-content-handlers
        #:add-request-adjuster request-adjuster
        #:add-response-listener response-listener
        #:add-cookie-jar cookie-jar))
