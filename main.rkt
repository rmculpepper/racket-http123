;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/class
         racket/contract/base
         "private/interfaces.rkt"
         "private/request.rkt"
         "private/header.rkt"
         "private/response.rkt"
         "private/client-base.rkt"
         "private/client.rkt"
         "private/util.rkt")
(provide (struct-out exn:fail:http123)
         (struct-out request)
         ok-http-url?
         http-client
         header<%>
         header-key-symbol?
         http-client-base<%>
         http-client<%>
         http-response<%>)

(define (http-client #:add-header [add-header null])
  (define c (new http-client%))
  (send c fork #:add-header add-header))
