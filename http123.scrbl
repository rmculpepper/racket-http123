#lang scribble/manual
@(require scribble/example
          (for-label racket/base racket/contract racket/class
                     net/url-structs net/url-string
                     http123))

@(define (rfc7230 fragment . content)
   (apply hyperlink (format "https://tools.ietf.org/html/rfc7230#~a" fragment) content))

@(define (rfc7231 fragment . content)
   (apply hyperlink (format "https://tools.ietf.org/html/rfc7231#~a" fragment) content))

@(define (h2rfc fragment . content)
   (apply hyperlink (format "https://tools.ietf.org/html/rfc7540#~a" fragment) content))

@(begin
  (define the-eval (make-base-eval))
  (the-eval '(require http123)))

@title{http123: HTTP Client}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodule[http123]


@; ------------------------------------------------------------
@section[#:tag "exn"]{Exceptions}

@defstruct*[(exn:fail:http123 exn:fail) ([info (hash/c symbol? any/c)])]{

Represents an @deftech{HTTP error}---a protocol or communication error.

Here are some common keys in @racket[info]. The presence or absence of any of
these keys depends on the specific error.
@itemlist[

@item{@racket['version] --- either @racket['http/1.1] or @racket['http/2]}

@item{@racket['code] --- a symbol indicating what kind of error occurred, in a
form more suitable for comparison than parsing the error message; the following
is an incomplete list of values that indicate that more information is present
in the @racket['http2-error] key:
@itemlist[
@item{@racket['ua-connection-error] --- The user agent (this library) closed the
connection.}
@item{@racket['ua-stream-error] --- The user agent (this library) closed the
stream, but not necessarily the connection.}
@item{@racket['RST_STREAM] --- The server closed the stream, but not necessarily
the connection.}
@item{@racket['GOAWAY] --- The server closed the connection.}
]}

@item{@racket['http2-error] --- a symbol indicating an http/2
@h2rfc["section-7"]{error code} (eg, @racket['PROTOCOL_ERROR]) or
@racket['unknown] if the error code is unfamiliar}

@item{@racket['request] --- the @racket[request] that the error corresponds to}

@item{@racket['received] --- one of @racket['yes], @racket['no], or
@racket['unknown], indicating whether the request was received and processed by
the server}

@item{@racket['wrapped-exn] --- contains a more specific exception that
represents the immediate source of the error}

]}


@; ------------------------------------------------------------
@section[#:tag "request"]{Requests}

@defstruct*[request
            ([method symbol?]
             [url (or/c string? ok-http-url?)]
             [header (listof (or/c bytes? string? (list/c bytes? bytes?)))]
             [data (or/c #f bytes? (-> (-> bytes? void?) any))])]{

Represents an HTTP request. The constructor checks and normalizes its arguments
according to the following rules:
@itemlist[

@item{If @racketidfont{url} is a string, it is converted to a URL struct
(@racket[url?]). The URL must satisfy the constraints of @racket[ok-http-url?];
otherwise, an error is raised.}

@item{The @racket[header] normalized to a list of field ``entries''; each entry
has the form @racket[(list _key-bytes _value-bytes)], where @racket[_key-bytes]
is a valid header field name, and @racket[_value-bytes] is a valid header field
value. If a field is not well-formed, then an error is raised.}
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

@defmethod[(get-table) (hash/c header-key-symbol? (or/c bytes? (listof bytes?)))]{

Gets the mutable hash table storing the header's fields. Changes to this table
change the header's fields.
}

@defmethod[(get-header-entries) (listof (list/c bytes? bytes?))]{

Gets the header fields as a list of field entries, where each entry has the form
@racket[(list _key-bytes _value-bytes)]. The ordering of different keys in the
resulting list is not specified.

If a given @racket[_key] has multiple values, the result list has multiple
entries for that key---that is, values are not automatically combined. The
result list preserves the order of values for a given key.
}

@defmethod[(get-header-lines) (listof bytes?)]{

Gets the header fields as a list of field lines. The ordering of different keys
in the resulting list is not specified.

If a given @racket[_key] has multiple values, the result list has multiple
entries for that key---that is, values are not automatically combined. The
result list preserves the order of values for a given key.
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
@racket[#f] if the neader does not contain a field named @racket[key].

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
@rfc7230["appendix-B"]{token}; in addition, the symbol form must not have
upper-case letters.

@examples[#:eval the-eval
(header-key-symbol? 'content-length)
(header-key-symbol? 'Content-Length)
(header-key-symbol? #"Content-Length")
]}


@; ------------------------------------------------------------
@section[#:tag "response"]{Responses}

@definterface[http-response<%> ()]{

Represents an HTTP response (either HTTP/1.1 or HTTP/2).

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

Returns the response's @rfc7230["section-3.2"]{header}.
}

@defmethod[(has-content?) boolean?]{

Returns @racket[#t] if the response has a @rfc7230["section-3.3"]{message body},
@racket[#f] otherwise.
}

@defmethod[(get-content-in) (or/c #f input-port?)]{

If the response includes a @rfc7230["section-3.3"]{message body}, returns an
input port that reads from the message body. If the response does not contain a
message body, returns @racket[#f].

If the response's @tt{Content-Encoding} is either @tt{gzip} or @tt{deflate}, the
message body is automatically decompressed, and the result input port reads from
the uncompressed content.

Reading from the input port may raise an exception reflecting an error reading
the response or decompressing the content.

It is not necessary to close the resulting input port.
}

@defmethod[(get-trailer) (or/c #f (is-a?/c header<%>))]{

Returns the response's @rfc7230["section-4.1.2"]{trailer}, or @racket[#f] if no
trailer exists (for example, if an HTTP/1.1 response did not use chunked
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
@section[#:tag "client"]{HTTP Client}

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

@definterface[http-client<%> (http-client-base<%>)]{

@defmethod[(sync-request [req request?]) (is-a?/c http-response<%>)]{

Sends an HTTP request and returns the response.

Equivalent to
@racket[((sync (send @#,(this-obj) @#,method[http-client-base<%> async-request] req)))].
}
}


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
