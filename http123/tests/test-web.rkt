;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/class
         rackunit
         json
         http123)

;; Test using a few real live web servers.

(module test racket/base
  (printf "Skipping; run manually with `racket test-web.rkt`.\n"))

(define client
  (http-client #:add-header
               `([x-racket-version ,(version)])
               #:add-content-handlers
               `([application/json ,read-json])))

(test-case "OpenLibrary"
  ;; eg https://openlibrary.org/books/OL27372351M/How_to_Design_Programs
  (define books '(("OL27372351M" "How to Design Programs")
                  ("OL981817M" "LISP in small pieces")
                  ("OL1547287M" "Compiling with continuations")
                  ("OL9468435M" "Types and Programming Languages")))
  (define prefix "https://openlibrary.org/books/")
  (define resultv (make-vector (length books)))
  (define work-threads
    (for/list ([index (in-naturals)] [entry (in-list books)])
      (define req (request 'GET (string-append prefix (car entry) ".json")))
      (thread (lambda () (vector-set! resultv index (send client handle req))))))
  (for-each thread-wait work-threads)
  (for ([result (in-vector resultv)] [entry (in-list books)])
    (check-pred jsexpr? result)
    (check-equal? (hash-ref result 'title #f) (cadr entry))))

(test-case "xkcd"
  ;; https://xkcd.com/json.html
  (define comics '((327 "Exploits of a Mom")
                   (297 "Lisp Cycles")
                   (208 "Regular Expressions")
                   (303 "Compiling")
                   (2347 "Dependency")))
  (define resultv (make-vector (length comics)))
  (define work-threads
    (for/list ([index (in-naturals)] [entry (in-list comics)])
      (define req (request 'GET (format "https://xkcd.com/~a/info.0.json" (car entry))))
      (thread (lambda () (vector-set! resultv index (send client handle req))))))
  (for-each thread-wait work-threads)
  (for ([result (in-vector resultv)] [entry (in-list comics)])
    (check-pred jsexpr? result)
    (check-equal? (hash-ref result 'title #f) (cadr entry))))

;; Currently (3/2025), tools.ietf.org redirects to www.rfc-editor.org
;; https://tools.ietf.org/rfc/rfc7540.txt
;; https://www.rfc-editor.org/rfc/rfc7540.txt
(define RFC-URL "https://www.rfc-editor.org/rfc/rfc7540.txt")

(test-case "fail rfc-editor.org"
  (check-exn #rx"no content handler matched"
             (lambda () (send client handle (request 'GET RFC-URL)))))

(define client2
  (send client fork
        #:add-content-handlers
        `([text/plain ,(lambda (in) (read-string 40 in))]
          [*/* ,(lambda (in) (list (send (current-response) get-content-type)))])))

(test-case "ok ietf"
  (check-pred string? (send client2 handle (request 'GET RFC-URL))))

(test-case "ok google"
  (check-equal? (send client2 handle (request 'GET "https://www.google.com/"))
                '(text/html)))

(test-case "fail mirror"
  (check-exn #rx"no response handler matched"
             (lambda ()
               (send client2 handle
                     (request 'GET "https://mirror.racket-lang.org/no-such-file.html")))))

(define client3
  (send client2 fork
        #:add-response-handlers
        `([404 ,(lambda (client resp) 'not-found)]
          [client-error ,(lambda (client resp) 'failed)])))

(test-case "ok 403"
  (check-equal? (send client3 handle (request 'GET "https://racket-lang.org/secret-plans.scrbl"))
                'failed))
(test-case "ok 404"
  (check-equal? (send client3 handle
                      (request 'GET "https://mirror.racket-lang.org/no-such-file.html"))
                'not-found))
