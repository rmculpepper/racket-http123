;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/contract/base
         racket/match
         racket/random
         net/base64
         net/url-structs
         json
         net/uri-codec
         "../private/interfaces.rkt"
         "../private/io.rkt"
         "../private/header-base.rkt"
         "../private/request.rkt")
(provide (contract-out
          [request/json
           (->* [method/c
                 (or/c string? url?)
                 (listof in-header-field/c)
                 jsexpr?]
                [#:write? boolean?]
                request?)]
          [request/form-urlencoded
           (-> method/c
               (or/c string? url?)
               (listof in-header-field/c)
               (listof (cons/c symbol? (or/c #f string?)))
               request?)]
          [request/multipart
           (->* [method/c
                 (or/c string? url?)
                 (listof in-header-field/c)
                 (listof part/c)]
                [#:write? boolean?]
                request?)]))

(define method/c (apply or/c '(GET HEAD POST PUT DELETE OPTIONS TRACE PATCH)))

(define (request/json method url header jsexpr #:write? [write? #f])
  (with-entry-point 'request/json
    (request-update (request method url header #f)
                    #:add-header '((#"content-type" #"application/json"))
                    #:set-data (cond [write? (lambda (out) (write-json jsexpr out))]
                                     [else (jsexpr->bytes jsexpr)]))))

(define (request/form-urlencoded method url header form-alist)
  (with-entry-point 'request/form-urlencoded
    (request-update (request method url header #f)
                    #:add-header '((#"content-type" #"application/x-www-form-urlencoded"))
                    #:set-data (alist->form-urlencoded form-alist))))

(define (request/multipart method url header parts #:write? [write? #t])
  (define boundary (base64-encode (crypto-random-bytes 30) #""))
  (request-update  (request method url header #f)
                   #:add-header `((#"content-type"
                                   ,(format "multipart/form-data; boundary=~a" boundary)))
                   #:set-data (make-multipart-data parts boundary)))

(define name/c (or/c string? symbol? bytes?))
(define value/c (or/c string? bytes? procedure?))
(define part/c
  (or/c (list/c name/c value/c)
        (list/c name/c value/c (listof in-header-field/c))
        (list/c name/c value/c (listof in-header-field/c) '#:filename name/c)))

(define ((make-multipart-data parts boundary) out)
  (for ([part (in-list parts)])
    (match part
      [(list name value)
       (write-part-prefix out boundary name null #f)
       (write-part-value out value)]
      [(list name value part-header)
       (write-part-prefix out boundary name part-header #f)
       (write-part-value out value)]
      [(list name value part-header #:filename filename)
       (write-part-prefix out boundary name part-header filename)
       (write-part-value out value)]))
  (write-multipart-terminator out boundary))

(define (write-part-prefix out boundary name header filename)
  (fprintf out "\r\n--~a\r\n" boundary)
  (let ([enc-name (uri-encode name)]
        [enc-filename (and filename (uri-encode filename))])
    (if enc-filename
        (fprintf out "content-disposition: form-data; name=~s; filename=~s" enc-name enc-filename)
        (fprintf out "content-disposition: form-data; name=~s" enc-name)))
  (for ([hfield (in-list header)])
    (fprintf "~a: ~a\r\n" (car hfield) (cadr hfield)))
  (fprintf out "\r\n"))

(define (write-part-value out value)
  (cond [(bytes? value) (write-bytes value out)]
        [(string? value) (write-string value out)]
        [(procedure? value)
         (define proxy-out (proxy-output-port out))
         (value proxy-out)
         (close-output-port proxy-out)]))

(define (write-multipart-terminator out boundary)
  (fprintf out "\r\n--~a--\r\n" boundary))
