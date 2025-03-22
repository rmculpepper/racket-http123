;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang info

;; ========================================
;; pkg info

(define collection "http123")
(define deps
  '("base"
    "binaryio-lib"
    ["scramble-lib" #:version "0.5"]
    "net-cookies-lib"))
(define build-deps
  '("rackunit-lib"))
(define pkg-authors '(ryanc))

;; ========================================
;; collect info

(define name "http123")
