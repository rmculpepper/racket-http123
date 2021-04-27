#lang scribble/manual
@(require "util.rkt"
          (for-label racket/base racket/contract racket/class
                     net/url http123))

@; ------------------------------------------------------------
@title[#:tag "response"]{Responses}

@definterface[response<%> ()]{

Represents an HTTP @deftech{response} (either http/1.1 or http/2).

The response object is created after successfully receiving the response status
and header; it does not imply that the response's message body was successfully
read. The user may receive the response object while reading of the message body
proceeds concurrently. See @method[response<%> get-content-in] and
@method[response<%> get-trailer-evt] for notes on exceptions regarding errors in
the response occurring after the header.

@defmethod[(get-version) (or/c 'http/1.1 'http/2)]{

Returns a symbol identifying version of HTTP used to retrieve the response.

Note: Future versions of this library may add more possible results.
}

@defmethod[(get-status-code) (integer-in 100 599)]{

Returns the response's @rfc7231["section-6"]{status code}.

Note: this library currently discards all Informational (1xx) responses.
}

@defmethod[(get-status-class) status-class/c]{

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

@defmethod[(get-content-in [port-if-no-body? boolean? #f])
           (or/c #f input-port?)]{

If the response includes a @h11rfc["section-3.3"]{message body}, returns an
input port that reads from the message body. If the response does not contain a
message body, then if @racket[port-if-no-body?] is true, returns an empty input
port, otherwise returns @racket[#f].

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

This method blocks until the response has been fully received; see also
@method[response<%> get-trailer-evt]. This method may raise an exception
reflecting an error reading the response.
}

@defmethod[(get-trailer-evt) (evt/c (-> (or/c #f (is-a?/c header<%>))))]{

Returns an event that is ready for synchronization when the response has been
fully received. The synchronization result is a procedure that returns the
trailer if there was one, returns @racket[#f] if there was no trailer, or raises
an exception if there was an error reading the response. See also
@secref["evt-result"].
}
}

@defthing[status-class/c contract?
          #:value (or/c 'informational 'successful 'redirection 'client-error 'server-error)]{

Contract for symbols representing a response's @rfc7231["section-6"]{status class}.
}
