#lang scribble/manual
@(require "util.rkt"
          (for-label racket/base racket/contract racket/class
                     net/url http123))

@title[#:tag "notes"]{Notes}

@; ------------------------------------------------------------
@section[#:tag "known-issues"]{Known Issues and Limitations}

Support for @(HTTP/2) requires Racket 8.1 or later, because previous versions of
Racket's @racketmodname[openssl] bindings do not support ALPN.

The following features are currently unsupported:
@itemlist[

@item{@(HTTP/2) without TLS (aka, @h2rfc["section-3.2"]{``h2c''})}

@item{the @rfc7231["section-4.3.6"]{@tt{CONNECT}} method}

@item{the @tt{Upgrade} header field (@(HTTP/1.1)) --- Note: the @(HTTP/2)
protocol @h2rfc["section-8.1.2.2"]{disallows} @tt{Upgrade}.}

@item{Informational (1xx) responses --- This library silently discards
@rfc7231["section-6.2"]{Informational (1xx) responses}.}

@item{the @tt{Expect: 100-continue} header field --- It is allowed, but this
library ignores any @tt{100 Continue} response (see previous), and it never
delays sending the request message body.}

@item{``server push'' streams (@tt{PUSH_PROMISE}) (@(HTTP/2)) --- The client's
initial @tt{SETTINGS} frame at connection startup disables the feature.}

@item{stream @h2rfc["section-5.3"]{priorities} (@(HTTP/2))}

@item{various limits on protocol elements, with reasonable defaults}

]

There are various things that should be configurable that currently are not. A
few examples:
@itemlist[

@item{flow control window sizes (@(HTTP/2))}

@item{HPACK indexing policy, including @hpackrfc["section-7.1.3"]{never-index
fields} (@(HTTP/2))}

]

@; ------------------------------------------------------------
@section[#:tag "log"]{Logging}

This library logs on the following topics:
@itemlist[

@item{@tt{http} --- about high-level client operations, connection creation and
management, and dispatching to @(HTTP/1.1) or @(HTTP/2) implementations}

@item{@tt{http1} --- about connections, request, and responses using the
@(HTTP/1.1) protocol}

@item{@tt{http2} --- about connections, streams, requests, and responses using
the @(HTTP/2) protocol}

]

@; ------------------------------------------------------------
@section[#:tag "misc"]{Miscellaneous Notes}

A client automatically adds the following header fields, unless they overridden
by user-supplied fields:
@itemlist[
@item{@tt{Accept-Encoding: gzip, deflate}}
@item{@tt{User-Agent: racket-http123/}@emph{lib-version}}
]

In general, the functions and methods of this library are thread-safe but not
kill-safe. For example, killing a thread that is using a client object has a
small chance of damaging the client object so that future operations block
forever.

Responses are not bound to specific clients. It may be useful to fetch a
response with one client and handle it (or handle redirections) with another
client.


@; ------------------------------------------------------------
@section[#:tag "evt-result"]{Synchronizable Event Results}

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
@section[#:tag "hpack-indexing"]{HPACK Indexing Policy}

HPACK (the compression scheme for @(HTTP/2) headers) is designed to reduce
vulnerability to attacks like @hyperlink["https://en.wikipedia.org/wiki/CRIME"]{CRIME},
which can discover secrets in headers by injecting data into the header and
observing the effectiveness of header compression. Part of this defense is
intrinsic, but HPACK additionally allows senders to choose which header fields
are ``indexed''---that is, entered into the dynamic compression table, to
further protect secret header data. For example, some @(HTTP/2) client libraries
automatically mark @tt{Authorization} fields and short @tt{Cookie} fields as
not-indexed, since they might contain low-entropy secret data.

This library indexes @tt{Authorization} and @tt{Cookie} fields by default. In
general, it does not mark any header fields as not-indexed based on security
rationale, although it does avoid indexing for other reasons. For example, it
does not index @tt{If-Modified-Since} header fields, since they are unlikely to
have the same value from request to request.

Users of this library should follow this policy: If a request has a header field
with secret data, then the user should not allow an untrusted source to
influence the value of the same header field in any other request made on the
same connection. For example, if you make (or might make) a request with an
@tt{Authorization} header field containing your password, you should not make
any other requests on the same connection with an @tt{Authorization} header
whose value is influenced by an untrusted party.

Future versions of this library may make the indexing policy configurable.

See the @hpackrfc["section-7"]{HPACK Security Considerations} for more details.
