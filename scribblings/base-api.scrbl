#lang scribble/manual
@(require scribble/example
          racket/list
          racket/runtime-path
          "util.rkt"
          (for-label racket/base racket/contract racket/class
                     net/url-structs net/url-string
                     http123))

@(begin
  (define-runtime-path log-file "log-base-api.rktd")
  (define the-eval (make-log-based-eval log-file 'record))
  (the-eval '(require http123 racket/class racket/port racket/pretty
                      (submod http123/private/util pretty))))

@title[#:tag "base-api"]{Basic API}

@; ------------------------------------------------------------
@section[#:tag "request"]{Requests}

@defstruct*[request
            ([method (or/c 'GET 'HEAD 'POST 'PUT 'DELETE 'OPTIONS 'TRACE 'PATCH)]
             [url (or/c string? ok-http-url?)]
             [header (listof (or/c bytes? string?
                                   (list/c (or/c symbol? string? bytes?)
                                           (or/c string? bytes?))))]
             [data (or/c #f bytes? (-> (-> bytes? void?) any))])]{

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
@section[#:tag "response"]{Responses}

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


@; ------------------------------------------------------------
@section[#:tag "header"]{Headers}

@definterface[header<%> ()]{

Interface for objects representing @rfc7231["section-7"]{response headers} and
@h11rfc["section-4.1.2"]{trailers}.

Note on terminology: a request or response contains a single @emph{header},
which consists of zero or more @emph{header fields}.

@defmethod[(get-fields) (listof (list/c bytes? bytes?))]{

Gets a list of header field entries, where each entry has the form @racket[(list
_key-bytes _value-bytes)]. The ordering of different keys in the resulting list
is not specified.

If a given @racket[_key] has multiple values, the result list has multiple
entries for that key---that is, values are not combined. The result list
preserves the order of values for a given key.
}

@defmethod[(get-field-lines) (listof bytes?)]{

Gets a list of header field lines. The ordering of different keys in the
resulting list is not specified.

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
]}

@; ------------------------------------------------------------
@(close-eval the-eval)
