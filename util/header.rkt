;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/contract/base
         racket/match
         "../private/interfaces.rkt"
         (rename-in "../private/header-base.rkt"
                    [check-header-field h:check-header-field]))

(provide in-header-field/c
         header-field/c
         (contract-out
          [check-header-field
           (-> in-header-field/c
               header-field/c)]
          [header-field-list-update
           (-> (listof header-field/c)
               (listof header-field/c)
               (listof header-field/c))]))

(define (check-header-field hfs)
  (with-entry-point 'check-header-field
    (h:check-header-field hfs)))
