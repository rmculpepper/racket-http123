;; Copyright 2020-2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/match
         racket/list
         racket/contract/base
         net/uri-codec
         net/url-structs
         net/url-string
         "../private/util.rkt")
(provide ok-http-url?
         (contract-out
          [build-url
           (->* [(or/c url? string?)]
                [#:query (listof (cons/c symbol? (or/c #f string?)))
                 #:fragment (or/c #f string?)]
                #:rest (listof (or/c path/param? string?))
                url?)]))

(define (build-url base
                   #:query [query-parts null]
                   #:fragment [fragment #f]
                   . path-parts)
  (build-url* base path-parts query-parts fragment))

(define (build-url* base path-parts query-parts fragment)
  (match base
    [(? string?)
     (build-url* (string->url base) path-parts query-parts fragment)]
    [(url scheme user host port path-absolute? path query old-fragment)
     (define path*
       (if (pair? path-parts)
           (simplify (apply append path (map build-path/params path-parts)))
           path))
     (define query* (append query query-parts))
     (define fragment* (or fragment old-fragment))
     (url scheme user host port path-absolute? path* query* fragment*)]))

(define same-pp (path/param 'same null))
(define up-pp (path/param 'up null))
(define empty-pp (path/param "" null))

(define (build-path/params p)
  (cond [(path/param? p) (list p)]
        [else (for/list ([s (in-list (regexp-split #rx"/" p))])
                (path/param (match s ["." 'same] [".." 'up] [path path]) null))]))

(define (url-simplify-path u)
  (match u
    [(? string?)
     (url-simplify-path (string->url u))]
    [(url scheme user host port path-absolute? path query fragment)
     (url scheme user host port path-absolute? (simplify path) query fragment)]))

(define (simplify pps)
  (define (loop pps acc slash?)
    (match pps
      [(list* (and up (path/param 'up '())) rest-pps)
       (match acc
         [(list* (path/param _ '()) rest-acc)
          (loop rest-pps rest-acc slash?)]
         [_ (append (reverse (cons up acc)) (loop rest-pps null #f))])]
      [(list* (path/param 'same '()) rest-pps)
       (loop rest-pps acc slash?)]
      [(list* (and np (path/param "" '())) rest-pps)
       (loop rest-pps acc #t)]
      [(list* pp rest-pps)
       (loop rest-pps (cons pp acc) #f)]
      ['()
       (cond [slash? (reverse (cons empty-pp acc))]
             [else (reverse acc)])]))
  (define (simplify* pps)
    (loop pps null))
  (define (simplifiable? pp)
    (match pp
      [(path/param 'up '()) #t]
      [(path/param 'same '()) #t]
      [(path/param "" '()) #t]
      [_ #f]))
  (if (ormap simplifiable? pps)
      (loop pps null #f)
      pps))

(module+ test
  (equal? (url->string (url-simplify-path "a/b/c")) "a/b/c")
  (equal? (url->string (url-simplify-path "a//b/c")) "a/b/c")
  (equal? (url->string (url-simplify-path "a/./b/./c")) "a/b/c")
  (equal? (url->string (url-simplify-path "a/z/y/../..//b/c")) "a/b/c")
  (equal? (url->string (url-simplify-path "../a/b/c")) "../a/b/c")
  (equal? (url->string (url-simplify-path "..//a//b/c")) "../a/b/c")
  (equal? (url->string (url-simplify-path "a/../../b/c")) "../b/c")

  (equal? (url->string (url-simplify-path "a///b/c/")) "a/b/c/")
  (equal? (url->string (url-simplify-path "a/b/c//")) "a/b/c/")
  (equal? (url->string (url-simplify-path "a/b/c/./")) "a/b/c/")
  (equal? (url->string (url-simplify-path "a/b/c//.")) "a/b/c/")

  (equal? (url->string (build-url "a/b/c/")) "a/b/c/")
  (equal? (url->string (build-url "a/b/c/" "")) "a/b/c/")
  (equal? (url->string (build-url "a/b/c/" "/")) "a/b/c/")
  (equal? (url->string (build-url "a/b/c/" ".")) "a/b/c/")
  (equal? (url->string (build-url "a/b/c/" "..")) "a/b/"))
