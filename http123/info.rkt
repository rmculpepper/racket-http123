;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang info

;; ========================================
;; pkg info

(define collection "http123")
(define deps
  '("base"
    "http123-lib"))
(define build-deps
  '("racket-doc"
    "rackunit-lib"
    "scribble-lib"
    "net-doc"
    "net-cookies-lib"
    "net-cookies-doc"
    "web-server-lib"))
(define implies '("http123-lib"))
(define pkg-authors '(ryanc))

;; ========================================
;; collect info

(define name "http123")
(define scribblings
  '(["scribblings/http123.scrbl" (#;multi-page)]))
