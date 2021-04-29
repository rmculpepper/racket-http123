#lang scribble/manual
@(require scribble/example
          racket/list
          racket/runtime-path
          "util.rkt"
          (for-label racket/base racket/contract racket/class net/url http123))

@(begin
  (define-runtime-path log-file "log-request.rktd")
  (define the-eval (make-log-based-eval log-file 'replay))
  (the-eval '(require http123 racket/class racket/port racket/pretty
                      net/url-string
                      (submod http123/scribblings/util pretty))))

@; ------------------------------------------------------------
@title[#:tag "request"]{Requests}

@defstruct*[request
            ([method (or/c 'GET 'HEAD 'POST 'PUT 'DELETE 'OPTIONS 'TRACE 'PATCH)]
             [url (or/c string? ok-http-url?)]
             [header (listof in-header-field/c)]
             [data (or/c #f bytes? (-> output-port? any))])]{

Represents an HTTP @deftech{request}.

The @racket[method] field indicates the @rfc7231["section-4"]{request
method}. Only the methods listed in the contract above are currently allowed.

The @racketidfont{url} field contains the @h11rfc["section-5.3"]{request
target}. It must be given in absolute form (see the notes below about checks and
conversions performed by the constructor).

The @racket[header] field contains the @rfc7231["section-5"]{request header} as
a list of header fields. A header field may be given in either of the following
forms:
@itemlist[

@item{a string or byte string containing both the field name and value --- for
example, @racket["User-Agent: racket-http123/0.1"]}

@item{a list @racket[(list _key _value)] --- for example,
@racket['(accept-encoding "gzip, deflate")] or @racket['(#"accept-encoding"
#"gzip, deflate")]}

]

The @racket[data] field contains the request @h11rfc["section-3.3"]{message
body}. The @racket[data] field must be one of the following forms:
@itemlist[

@item{If @racket[data] is @racket[#f], the request has no body.}

@item{If @racket[data] is a byte string, it is sent as the message body (and
when using http/1.1, a @tt{Content-Length} header field will be added
automatically).}

@item{If @racket[data] is a procedure, it is called with an output port to
incrementally produce the message body (and when using http/1.1, a
@tt{Transfer-Encoding: chunked} header field will be added automatically). When
the output port is closed, the message body is complete; the output port is also
closed automatically when the call to @racket[data] returns. If the call to
@racket[data] raises an exception, the request is canceled (but the server may
have already started processing it).}

]

The constructor checks and converts its arguments according to the following
rules:
@itemlist[

@item{If @racketidfont{url} is a string, it is converted to a URL struct
(@racket[url?]). The URL must satisfy the constraints of @racket[ok-http-url?];
otherwise, an error is raised.}

@item{The @racket[header] is normalized to a list of field entries, where each
entry has the form @racket[(list _key-bytes _value-bytes)], where
@racket[_key-bytes] is a valid header field name with no uppercase letters, and
@racket[_value-bytes] is a valid header field value. If a field is not
well-formed, then an error is raised.}

@item{If @racket[header] contains one of the following header fields, an
exception is raised (these header fields are reserved for control by the user
agent):
@(add-between (map (lambda (s) (tt (symbol->string s)))
                   '(Host Content-Length Connection Leep-Alive Upgrade
                     Transfer-Encoding TE Trailer))
              ", ").}

]

@examples[#:eval the-eval
(eval:alts
 (request 'HEAD "https://blog.racket-lang.org/"
          '("If-Modified-Since: Sun, 14 Feb 2021 01:00:00 GMT"))
 (P (request 'HEAD "https://blog.racket-lang.org/"
             '("If-Modified-Since: Sun, 14 Feb 2021 01:00:00 GMT"))))
]}

@defproc[(ok-http-url? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a URL structure (@racket[url?]) that
satisfies the following constraints, @racket[#f] otherwise. The constraints are:
@itemlist[

@item{the scheme is @racket["http"] or @racket["https"] (compared case-insensitively)}
@item{the user field is absent (@racket[#f])}
@item{the host field is present}
@item{the path is marked as absolute}

]

@examples[#:eval the-eval
(ok-http-url? (string->url "http://racket-lang.org/"))
(ok-http-url? (string->url "http://ryanc@racket-lang.org/"))
(ok-http-url? (string->url "ftp://mirror.racket-lang.org/"))
(ok-http-url? (string->url "somewhere/out-there.html"))
]}


@; ------------------------------------------------------------
@(close-eval the-eval)
