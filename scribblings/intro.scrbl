#lang scribble/manual
@(require scribble/example
          racket/runtime-path
          "util.rkt"
          (for-label racket/base racket/contract racket/class
                     net/url json http123))

@(begin
  (define-runtime-path log-file "log-intro.rktd")
  (define the-eval (make-log-based-eval log-file 'replay))
  (the-eval '(require http123 racket/class racket/port racket/pretty json
                      (submod http123/scribblings/util pretty))))

@; ------------------------------------------------------------
@title[#:tag "intro"]{Introduction to http123}

@section[#:tag "client-intro"]{Using the Client API}

This section introduces the high-level, handlers-based client API.

Create a client with default header fields and content handlers for
expected content types:
@examples[#:eval the-eval #:label #f
(define client
  (http-client #:add-header
               `([x-racket-version ,(version)])
               #:add-content-handlers
               `([application/json ,read-json])))
]
Header fields can be specified in several different forms.

Construct a request with a method and URL:
@examples[#:eval the-eval #:label #f
(define time-req (request 'GET "http://date.jsontest.com/"))
]
A request can also contain a header and data (a message body). Fields
in the request header override any fields of the same name in the
client's default header.

Execute the request and handle the response:
@examples[#:eval the-eval #:label #f
(send client handle time-req)
]
The @tt{Content-Type} of this response is @tt{application/json}, which
the client is configured to handle using @racket[read-json].

If the client has no handler for the response's content type, it
raises an exception. For example:
@examples[#:eval the-eval #:label #f
(eval:error
 (send client handle (request 'GET "https://tools.ietf.org/rfc/rfc7540.txt")))
]

You can create a new client with additional handlers by calling the
@method[http-client<%> fork] method. The resulting client shares its
connections with the original client.
@examples[#:eval the-eval #:label #f
(define client2
  (send client fork
        #:add-content-handlers
        `([text/plain ,(lambda (in) (read-string 40 in))]
          [*/* ,(lambda (in)
                  (format "something called ~s, I guess"
                          (send (current-response) get-content-type)))])))
(send client2 handle (request 'GET "https://tools.ietf.org/rfc/rfc7540.txt"))
(send client2 handle (request 'GET "https://www.google.com/"))
]

By default, the content handlers are only called for responses with
status code 200 (``Found''). The default response handler raises an
exception for any other response. Additional response handlers can be
added for individual status codes or status classes:
@examples[#:eval the-eval #:label #f
(define client3
  (send client2 fork
        #:add-response-handlers
        `([404 ,(lambda (client resp) 'not-found)]
          [client-error ,(lambda (client resp) 'failed)])))
(send client3 handle (request 'GET "https://racket-lang.org/secret-plans.scrbl"))
(send client3 handle (request 'GET "https://mirror.racket-lang.org/no-such-file.html"))
]


@section[#:tag "base-intro"]{Using the Response API}

This section introduces a lower-level client API that users can use to
simply retrieve response objects.

Create a client:
@examples[#:eval the-eval #:label #f
(define client (http-client))
]

Create a request:
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

Using @method[http-client<%> async-request] it is possible to submit
send requests and receive responses as they arrive. In particular, in
http/2 connections responses may arrive in an order different from the
order the requests were sent. Of course, responses using different
connections are always unordered.

The event returned by @method[http-client<%> async-request] produces a
thunk as its synchronization result; apply the thunk to get the
response or to raise an exception if there was an error getting the
response. (See @secref["evt-result"] for rationale.)
@examples[#:eval the-eval #:label #f
(define ietf-evt
  (send client async-request
        (request 'GET "https://tools.ietf.org/rfc/rfc7540.txt")))
(define google-evt
  (send client async-request
        (request 'GET "https://www.google.com/")))
(eval:alts
 ((sync ietf-evt google-evt))
 (let ([resp ((sync ietf-evt google-evt))])
   (define h (send resp get-header))
   (send h remove! 'alt-svc)
   (send h remove! 'set-cookie)
   (pretty (call-with-output-string
            (lambda (out) (pretty-print resp out))))))
]

@; ------------------------------------------------------------
@(close-eval the-eval)
