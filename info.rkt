;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang info

;; ========================================
;; pkg info

(define collection "http123")
(define deps
  '("base"
    "binaryio-lib"
    "scramble-lib"))
(define build-deps
  '("racket-doc"
    "scribble-lib"))

;; ========================================
;; collect info

(define name "http123")
(define scribblings
  '(["scribblings/http123.scrbl" (#;multi-page)]))
