;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/class
         racket/contract/base
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
         http-client
         http-client-base<%>
         http-client<%>
         current-response

         in-header-field/c
         header-field/c
         status-class/c
         response-handler/c
         content-handler/c)

(define (http-client #:add-header [new-header null]
                     #:add-response-handlers [new-response-handlers null]
                     #:add-content-handlers [new-content-handlers null])
  (define c (new http-client%))
  (send c fork
        #:add-header new-header
        #:add-response-handlers new-response-handlers
        #:add-content-handlers new-content-handlers))
