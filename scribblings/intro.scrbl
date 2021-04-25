#lang scribble/manual
@(require scribble/example
          racket/list
          racket/runtime-path
          "util.rkt"
          (for-label racket/base racket/contract racket/class
                     net/url-structs net/url-string
                     http123))

@(begin
  (define-runtime-path log-file "log-intro.rktd")
  (define the-eval (make-log-based-eval log-file 'replay))
  (the-eval '(require http123 racket/class racket/port racket/pretty
                      (submod http123/private/util pretty))))

@; ------------------------------------------------------------
@title[#:tag "intro"]{Introduction to http123}

@bold{Note: } This section describes how to use the library given the current
interfaces. When progress is made on the high-level client interface, this
section will be updated.

Create a client:
@examples[#:eval the-eval #:label #f
(define client (http-client))
]

Create a @racket[request]. A request combines a method, location, header, and
optional data. Header fields can be specified in several different forms.
@examples[#:eval the-eval #:label #f
(define header '("Accept-Encoding: gzip, deflate"
                 (accept-language #"en")
                 (#"User-Agent" #"racket-http123/0.1")))
(define req (request 'GET "https://www.google.com/" header #f))
]

Use @method[http-client<%> sync-request] to perform the request and get a
response:
@examples[#:eval the-eval #:label #f
(define resp (send client sync-request req))
(eval:alts
 @#,elem{@racket[(code:comment "... prune away some header fields ...")]}
 (begin (let ([h (send resp get-header)])
          (send h remove! 'alt-svc)
          (send h remove! 'set-cookie))))
(eval:alts
 resp
 (pretty (call-with-output-string
          (lambda (out) (pretty-print resp out)))))
(send resp get-status-code)
]

The response's content is available as an input port. If the response specifies
a @tt{Content-Encoding} of @tt{gzip} or @tt{deflate}, the content is
automatically decompressed.
@examples[#:eval the-eval #:label #f
(read-string 15 (send resp get-content-in))
]
Once read, content data is gone forever!
@examples[#:eval the-eval #:label #f
(read-string 5 (send resp get-content-in))
]

@; ------------------------------------------------------------
@(close-eval the-eval)
