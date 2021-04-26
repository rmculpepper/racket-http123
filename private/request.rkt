;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/match
         net/url-structs
         "interfaces.rkt"
         "header-base.rkt"
         "util.rkt")
(provide (all-defined-out))

;; A Request is:
(struct request
  (method       ;; Symbol, like 'GET
   url          ;; URL
   header       ;; HeaderFieldList
   data         ;; (U #f Bytes (OutputPort -> Void))
   )
  #:guard (lambda (method loc header data _)
            (unless (memq method known-method-names)
              (define parts
                (apply string-append (map (lambda (n) (format " '~a" n)) known-method-names)))
              (raise-argument-error 'request (format "(or/c~a)" parts) method))
            (define u
              (cond [(or (url? loc) (string? loc)) (check-http-url 'request loc loc)]
                    [else (raise-argument-error 'request "(or/c string? url?)" loc)]))
            (define hs
              (with-entry-point 'request
                (check-header-field-list header)))
            (for ([hfield (in-list hs)])
              (match-define (list key val) hfield)
              (when (member key reserved-header-keys/bytes)
                (h-error "request contains header field reserved for user-agent\n  field: ~e" key
                         #:info (hasheq 'code 'reserved-request-header-field))))
            (unless (or (eq? #f data) (bytes? data) (procedure? data))
              (raise-argument-error 'request "(or/c #f bytes? procedure?)" data))
            (values method u hs data))
  #:transparent
  #:property prop:about
  (lambda (self)
    (match-define (request method (url scheme _ host port _ _ _ _) _ _) self)
    (format "~a ~a://~a~a~a/..." method scheme host (if port ":" "") (or port ""))))

;; request:can-replay? : Request -> Boolean
;; Can the request be replayed in a different actual connection?  Only care
;; about request semantics, not effect on connection state (eg close, upgrade).
(define (request:can-replay? req)
  ;; FIXME: depends on policy, belongs in connection?
  (match req
    [(request method url header data)
     (and (method:idempotent? method)
          (or (eq? data #f) (bytes? data)))]))

;; ----------------------------------------

;; RFC 7231 4.2.1 (Safe Methods)
(define (method:safe? method)
  (memq method '(GET HEAD OPTIONS TRACE)))

;; RFC 7231 4.2.? (Idempotent Methods)
(define (method:idempotent? method)
  (or (method:safe? method) (memq method '(PUT DELETE))))

;; flags : safe, idempotent, {undef,forbid}-request-body, never-response-body
;; - 'undef-request-body - standard assigns no semantics, but not forbidden
;; - 'forbid-request-body - client MUST NOT send body
;; - 'never-response-body - no response body, regardless of status
(define known-methods
  #hasheq((GET     . (safe undef-request-body))
          (HEAD    . (safe undef-request-body never-response-body))
          (POST    . ())
          (PUT     . (idmp))
          (DELETE  . (idmp undef-request-body))
          (OPTIONS . (safe))
          (TRACE   . (safe forbid-request-body))
          (CONNECT . (disallow undef-request-body))

          (PATCH   . ()) ;; RFC 5789
          ))

(define known-method-names
  '(GET HEAD POST PUT DELETE OPTIONS TRACE PATCH))

;; ----------------------------------------

(define (request-update req
                        #:add-header [inhfields null]
                        #:set-data [new-data (request-data req)])
  (match-define (request method url header data) req)
  (define hfields (check-header-field-list inhfields))
  (define new-header (header-field-list-update header hfields))
  (request method url new-header new-data))

#|
(define (request/json method url header jsexpr)
  ;; FIXME: add delaying option?
  (request-update (request method url header #f)
                  #:add-header '((#"content-type" #"application/json"))
                  #:set-data (jsexpr->bytes jsexpr)))

(define (request/form-urlencoded method url header form-alist)
  (request-update (request method url header #f)
                  #:add-header '((#"content-type" #"application/x-www-form-urlencoded"))
                  #:set-data (alist->form-urlencoded form-alist)))

(define (request/multipart method url header parts)
  (define boundary (base64-encode (crypto-random-bytes 32)))
  (request-update  (request method url header #f)
                   #:add-header `((#"content-type"
                                   ,(format "multipart/form-data; boundary=~a" boundary)))
                   #:set-data (make-multipart-data parts boundary)))

(define (generate-boundary)
  (base64-encode (crypto-random-bytes 32)))

(define (write-multipart-field-part-prefix out boundary name
                                           #:header [hfields null])
  (fprintf out "\r\n--~a\r\n" boundary)
  (fprintf out "Content-Disposition: form-data; name=~s\r\n" name) ;; encode name?
  (for ([hfield (in-list hfields)])
    (fprintf "~a: ~a\r\n" (car hfield) (cadr hfield)))
  (fprintf out "\r\n"))

(define (write-multipart-file-part-prefix out boundary name filename
                                          #:header [hfields null])
  (fprintf out "\r\n--~a\r\n" boundary)
  (let ([enc-name (uri-encode name)]
        [enc-filename (and filename (uri-encode filename))])
    (if enc-filename
        (fprintf out "content-disposition: form-data; name=~s; filename=~s" enc-name enc-filename)
        (fprintf out "content-disposition: form-data; name=~s" enc-name)))
  (for ([hfield (in-list hfields)])
    (fprintf "~a: ~a\r\n" (car hfield) (cadr hfield)))
  (fprintf out "\r\n"))

(define (write-multipart-terminator out boundary)
  (fprintf out "\r\n--~a--\r\n" boundary))
|#
