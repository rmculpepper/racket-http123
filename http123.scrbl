#lang scribble/manual
@(require scribble/example
          racket/list
          racket/runtime-path
          (for-label racket/base racket/contract racket/class
                     net/url-structs net/url-string
                     http123))

@(define (h11rfc fragment . content)
   (apply hyperlink (format "https://tools.ietf.org/html/rfc7230#~a" fragment) content))

@(define (rfc7231 fragment . content)
   (apply hyperlink (format "https://tools.ietf.org/html/rfc7231#~a" fragment) content))

@(define (h2rfc fragment . content)
   (apply hyperlink (format "https://tools.ietf.org/html/rfc7540#~a" fragment) content))

@(begin
  (define-runtime-path log-file "private/eval-log.rktd")
  (define the-eval (make-log-based-eval log-file 'replay))
  (the-eval '(require http123 racket/class racket/port racket/pretty
                      (submod http123/private/util pretty))))

@title{http123: HTTP Client}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodule[http123]

Implements an @as-index{HTTP client} with support for both @as-index{http/1.1}
and @as-index{http/2} protocols.

@bold{Status: } The high-level client interface is unfinished. The http/1.1 and
http/2 protocol implementations are fairly complete, with some exceptions (see
@secref["known-issues"]).

@; ------------------------------------------------------------
@section[#:tag "intro"]{Introduction to http123}

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
@section[#:tag "client"]{HTTP Client}

@defproc[(http-client) (is-a?/c http-client<%>)]{

Creates an HTTP client.

The client automatically creates connections as necessary based on request URLs.
}

@definterface[http-client<%> (http-client-base<%>)]{

@defmethod[(sync-request [req request?]) (is-a?/c http-response<%>)]{

Sends an HTTP request and returns the response.

Equivalent to
@racket[((sync (send @#,(this-obj) @#,method[http-client-base<%> async-request] req)))].
}
}

@definterface[http-client-base<%> ()]{

@defmethod[(async-request [req request?]) (evt/c (-> (is-a?/c http-response<%>)))]{

Sends an HTTP request and returns a synchronizable event that is ready once one
of the following occurs:
@itemlist[

@item{The beginning of a response has been received, including the status and
header. The synchronization result is a constant function that returns an
instance of @racket[http-response<%>]; see @method[http-response<%>
get-content-in] for notes on concurrent processing of the response message
body.}

@item{The server closed the connection or sent an invalid response
beginning. The synchronization result is a function that raises an exception
when called.}

]

See @secref["evt-result"] for the rationale of the procedure wrapper.

An exception may be raised immediately if a failure occurs while sending the
request (for example, no connection could be made to the host).
}
}

@; ------------------------------------------------------------
@section[#:tag "request"]{Requests}

@defstruct*[request
            ([method (or/c 'GET 'HEAD 'POST 'PUT 'DELETE 'OPTIONS 'TRACE 'PATCH)]
             [url (or/c string? ok-http-url?)]
             [header (listof (or/c bytes? string?
                                   (list/c (or/c symbol? string? bytes?)
                                           (or/c string? bytes?))))]
             [data (or/c #f bytes? (-> (-> bytes? void?) any))])]{

Represents an HTTP request.

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

@item{If @racket[data] is a procedure, it is called with a procedure
@racket[_send-chunk] to incrementally produce the message body (and when using
http/1.1, a @tt{Transfer-Encoding: chunked} header field will be added
automatically). When the call to @racket[data] returns, the message body is
complete and further calls to @racket[_send-chunk] have no effect. If the call
to @racket[data] raises an exception, the request is cancelled (but the server
may have already started processing it).}

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

]}

@defproc[(ok-http-url? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a URL structure (@racket[url?]) that
satisfies the following constraints, @racket[#f] otherwise. The constraints are:
@itemlist[

@item{the scheme is @racket["http"] or @racket["https"] (compared case-insensitively)}
@item{the user field is absent (@racket[#f])}
@item{the host field is present}
@item{the path is marked as absolute}

]}



@; ------------------------------------------------------------
@section[#:tag "header"]{Headers}

@definterface[header<%> ()]{

Interface for objects representing @rfc7231["section-7"]{response headers} and
@h11rfc["section-4.1.2"]{trailers}.

Note on terminology: a request or response contains a single @emph{header},
which consists of zero or more @emph{header fields}.

@defmethod[(get-header-entries) (listof (list/c bytes? bytes?))]{

Gets the header fields as a list of field entries, where each entry has the form
@racket[(list _key-bytes _value-bytes)]. The ordering of different keys in the
resulting list is not specified.

If a given @racket[_key] has multiple values, the result list has multiple
entries for that key---that is, values are not combined. The result list
preserves the order of values for a given key.
}

@defmethod[(get-header-lines) (listof bytes?)]{

Gets the header fields as a list of field lines. The ordering of different keys
in the resulting list is not specified.

If a given @racket[_key] has multiple values, the result list has multiple
entries for that key---that is, values are not combined. The result list
preserves the order of values for a given key.
}

@defmethod[(has-key? [key header-key-symbol?]) boolean?]{

Returns @racket[#t] if the header contains a field named @racket[key],
@racket[#f] otherwise.
}

@defmethod[(get-values [key header-key-symbol?]) (or/c (listof bytes?) #f)]{

Returns the list of field values associated with @racket[key], or @racket[#f] if
the header does not contain a field named @racket[key].

The result list contains one element per field line in the original header. That
is, this method does not perform splitting or combination of field values.
}

@defmethod[(get-value [key header-key-symbol?]) (or/c bytes? #f)]{

Returns the value associated with @racket[key] as a single byte string, or
@racket[#f] if the header does not contain a field named @racket[key].

If the original header contained multiple lines for @racket[key], the values are
combined by concatenating them separated by @racket[#", "]. Use
@method[header<%> get-values] instead for fields that do not have
comma-separable values, such as @tt{Set-Cookie}.
}

@defmethod[(get-integer-value [key header-key-symbol?]) (or/c exact-integer? #f)]{

Returns the integer value associated with @racket[key]. If @racket[key] has no
value, or if the value is not a single exact integer, returns @racket[#f]
instead.
}
}

@defproc[(header-key-symbol? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a symbol that is a valid, canonical header
field name, @racket[#f] otherwise.

A valid header field name must match the grammar for
@h11rfc["appendix-B"]{token}; in addition, the symbol form must not have
upper-case letters.

@examples[#:eval the-eval
(header-key-symbol? 'content-length)
(header-key-symbol? 'Content-Length)
(header-key-symbol? #"Content-Length")
]}


@; ------------------------------------------------------------
@section[#:tag "response"]{Responses}

@definterface[http-response<%> ()]{

Represents an HTTP response (either http/1.1 or http/2).

The response object is created after successfully receiving the response status
and header; it does not imply that the response's message body was successfully
read. The user may receive the response object while reading of the message body
proceeds concurrently. See @method[http-response<%> get-content-in] and
@method[http-response<%> get-trailer-evt] for notes on exceptions regarding
errors in the response occurring after the header.

@defmethod[(get-version) (or/c 'http/1.1 'http/2)]{

Returns a symbol identifying version of HTTP used to retrieve the response.

Note: Future versions of this library may add more possible results.
}

@defmethod[(get-status-code) (integer-in 100 599)]{

Returns the response's @rfc7231["section-6"]{status code}.

Note: this library currently discards all Informational (1xx) responses.
}

@defmethod[(get-status-class) (or/c 'informational
                                    'successful
                                    'redirection
                                    'client-error
                                    'server-error)]{

Returns a symbol describing the @rfc7231["section-6"]{status class} of the
response's status code.

Note: this library currently discards all Informational (1xx) responses.
}

@defmethod[(get-header) (is-a?/c header<%>)]{

Returns the response's @h11rfc["section-3.2"]{header}.
}

@defmethod[(has-content?) boolean?]{

Returns @racket[#t] if the response has a @h11rfc["section-3.3"]{message body},
@racket[#f] otherwise.
}

@defmethod[(get-content-in) (or/c #f input-port?)]{

If the response includes a @h11rfc["section-3.3"]{message body}, returns an
input port that reads from the message body. If the response does not contain a
message body, returns @racket[#f].

If the response's @tt{Content-Encoding} is either @tt{gzip} or @tt{deflate}, the
message body is automatically decompressed, and the result input port reads from
the uncompressed content.

Reading from the input port may raise an exception reflecting an error reading
the response or decompressing the content.

It is not necessary to close the resulting input port.

When using http/2, the following behavior applies to the returned input port:
@itemlist[

@item{The unread content in the input port counts against the stream's flow
control window; after a certain amount of data is received, the server will not
be allowed to send more until data is consumed from the port.}

@item{If the input port is closed before the message body is completely
received, the user agent may attempt to cancel the stream to save network
traffic and processing.}

]}

@defmethod[(get-trailer) (or/c #f (is-a?/c header<%>))]{

Returns the response's @h11rfc["section-4.1.2"]{trailer}, or @racket[#f] if no
trailer exists (for example, if an http/1.1 response did not use chunked
transfer encoding).

This method blocks until the response has been fully received. See also
@method[http-response<%> get-trailer-evt]. It may raise an exception reflecting an
error reading the response.
}

@defmethod[(get-trailer-evt) (evt/c (-> (or/c #f (is-a?/c header<%>))))]{

Returns an event that is ready for synchronization when the response has been
fully received. The synchronization result is a procedure that returns the
trailer if there was one, returns @racket[#f] if there was no trailer, or raises
an exception if there was an error reading the response. See also
@secref["evt-result"].
}
}

@; ------------------------------------------------------------
@section[#:tag "exn"]{Exceptions}

@defstruct*[(exn:fail:http123 exn:fail) ([info (hash/c symbol? any/c)])]{

Represents an error related to this library, including protocol errors,
communication errors, and misues of library features.

The @racket[info] field contains an immutable hash with additional details about
the error. The following are some common keys in @racket[info]; the presence or
absence of any of these keys depends on the specific error.
@itemlist[

@item{@racket['version] --- either @racket['http/1.1] or @racket['http/2]}

@item{@racket['code] --- a symbol indicating what kind of error occurred, in a
form more suitable for comparison than parsing the error message}

@item{@racket['http2-error] --- a symbol indicating an http/2
@h2rfc["section-7"]{error code} (eg, @racket['PROTOCOL_ERROR]) or
@racket['unknown] if the error code is unfamiliar}

@item{@racket['request] --- the @racket[request] that the error is related to}

@item{@racket['received] --- one of @racket['yes], @racket['no], or
@racket['unknown], indicating whether the request was received and processed by
the server}

@item{@racket['wrapped-exn] --- contains a more specific exception that
represents the immediate source of the error}

]}



@; ------------------------------------------------------------
@section[#:tag "notes"]{Notes}

@subsection[#:tag "known-issues"]{Known Issues and Limitations}

The following features are currently unsupported:
@itemlist[

@item{http/2 without TLS (aka, ``h2c'')}
@item{the @tt{CONNECT} method}
@item{the @tt{Upgrade} header field (http/1.1) --- Note: the http/2 protocol
disallows @tt{Upgrade}.}
@item{Informational (1xx) responses --- This library silently discards
Informational (1xx) responses.}
@item{the @tt{Expect: 100-continue} header field --- It is allowed, but this
library ignores any @tt{100 Continue} response (see previous), and it never
delays sending the request message body.}
@item{``server push'' streams (@tt{PUSH_PROMISE}) (http/2) --- The client's
initial @tt{SETTINGS} frame at connection startup disables the feature.}
@item{stream priorities (http/2)}
@item{various limits on protocol elements, with reasonable defaults}

]

There are various things that should be configurable that currently are not. A
few examples:
@itemlist[

@item{flow control window sizes (http/2)}
@item{HPACK indexing policy, including never-index fields (http/2)}

]

@subsection[#:tag "log"]{Loggers}

This library logs on the following topics:
@itemlist[

@item{@tt{http} --- about high-level client operations, connection creation and
management, and dispatching to http/1.1 or http/2 implementations}

@item{@tt{http1} --- about connections, request, and responses using the
http/1.1 protocol}

@item{@tt{http2} --- about connections, streams, requests, and responses using
the http/2 protocol}

]

@subsection[#:tag "evt-result"]{Synchronizable Event Results}

This library uses @racket[(evt/c (-> _X))] to communicate the result of an
asynchronous process that normally produces an @racket[_X] but may fail. The
synchronization result is a constant function (or at least a function that
behaves like a constant function; calling it the first time may perform work,
but subsequent calls return the same result) in the success case, or a function
that raises an exception in the failure case.

An alternative would be @racket[(evt/c _X)]; this can be implemented on top of
the existing behavior by @racket[(wrap-evt evt (lambda (p) (p)))]. The
disadvantage to this approach is that it cannot be further wrapped with
exception-handling code. That is, a user that @racket[sync]s on @emph{multiple}
such events cannot tell which one raised the exception.

@; ------------------------------------------------------------
@(close-eval the-eval)
