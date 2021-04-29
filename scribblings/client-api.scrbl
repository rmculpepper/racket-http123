#lang scribble/manual
@(require "util.rkt"
          (for-label racket/base racket/contract racket/class
                     net/url http123 openssl net/cookies/user-agent))

@; ------------------------------------------------------------
@title[#:tag "client"]{Client API}

An HTTP client offers methods to perform @tech{requests} and handle
@tech{responses}.

@defproc[(http-client [#:ssl ssl (or/c 'secure 'auto ssl-client-context?) 'secure]
                      [#:add-header header-fields (listof header-field/c) null]
                      [#:add-response-handlers response-handlers
                       (listof
                        (list/c (or/c (integer-in 100-599) status-class/c)
                                response-handler/c))
                       null]
                      [#:add-content-handlers content-handlers
                       (listof (list/c symbol? content-handler/c))
                       null]
                      [#:add-request-adjuster request-adjuster
                       (or/c #f (-> request? request?))
                       #f]
                      [#:add-response-listener response-listener
                       (or/c #f (-> (is-a?/c response<%>) void?))
                       #f]
                      [#:add-cookie-jar cookie-jar
                       (or/c #f (is-a?/c cookie-jar<%>))
                       #f])
         (is-a?/c http-client<%>)]{

Creates a new HTTP client that does not share connections with any
existing clients.

The client automatically creates connections as necessary based on
request URLs. For @tt{https} requests, the client attempts to
negotiate an http/2 connection using ALPN; if the server does not
agree to http/2, the client falls back to http/1.1. For @tt{http}
requests, only http/1.1 is supported.

Connections created by the client are automatically closed after a
few seconds of inactivity.

The @racket[ssl] argument determines the client context used by
@racket[ssl-connect] when making @tt{https} connections.

See @method[http-client<%> fork] for an explanation of the other arguments.
}

@definterface[http-client<%> ()]{

A client contains the following:
@itemlist[

@item{a connection manager that manages existing connections and creates new ones on
demand}

@item{instructions for adjusting requests before they are executed (see
@method[http-client<%> adjust-request])---specifically, a base header and a list
of adjuster functions}

@item{instructions for handling responses once they are received (see
@method[http-client<%> handle-response])---specifically, a list of listeners, a
list of response handlers, and a list of content handlers (see
@method[http-client<%> handle-response-content])}

]

@defmethod[(fork [#:add-header header-fields (listof header-field/c) null]
                 [#:add-response-handlers response-handlers
                  (listof
                   (list/c (or/c (integer-in 100-599) status-class/c)
                           response-handler/c))
                  null]
                 [#:add-content-handlers content-handlers
                  (listof (list/c symbol? content-handler/c))
                  null]
                 [#:add-request-adjuster request-adjuster
                  (or/c #f (-> request? request?))
                  #f]
                 [#:add-response-listener response-listener
                  (or/c #f (-> (is-a?/c response<%>) void?))
                  #f]
                 [#:add-cookie-jar cookie-jar
                  (or/c #f (is-a?/c cookie-jar<%>))
                  #f])
           (is-a?/c http-client<%>)]{

Creates a new client object that shares the same connection manager as
@(this-obj), but adds the given header fields and handlers. The new header
fields and handlers take precedence over the existing header fields and
handlers. More precisely:
@itemlist[

@item{The header of the new client object consists of @racket[header-fields],
plus the header fields of @(this-obj) except for any fields whose keys also
occur in @racket[header-fields].}

@item{The response and content handlers of the new client consist of the
@racket[response-handlers] and @racket[content-handlers] prepended to the
existing response and content handlers, respectively.}

@item{The request adjusters of the new client consist of the new
@racket[request-adjuster] added to the existing request adjusters. That is, the
new adjuster is run before the existing adjusters.}

@item{The response listeners of the new client consist of the old listeners with
@racket[response-listener], if present, added to the end. That is, the new
response listener is run after the existing listeners.}

]

A @racket[cookie-jar] argument is converted to a request adjuster and a response
handler. The request adjuster adds @tt{Cookie} fields to the request header; the
response listener scans the response for @tt{Set-Cookie} fields and adds them to
@racket[cookie-jar]. (Beware, the default @racket[list-cookie-jar%]
implementation is not thread-safe. Access to @racket[cookie-jar] is synchronized
for @(this-obj) and derived clients, but not for other clients that
@racket[cookie-jar] is added to separately.)
}

@defmethod[(handle [req request?])
           any]{

Adjusts @racket[req] using the client's default header fields and adjusters (see
@method[http-client<%> adjust-request]), executes the request, notifies the
response listeners, and handles the response according to the client's response
and content handlers (see @method[http-client<%> handle-response] and
@method[http-client<%> handle-response-content]).

The result is the result of the selected response handler.

Equivalent to
@racketblock[
(send @#,(this-obj) @#,method[http-client<%> handle-response]
      (send @#,(this-obj) @#,method[http-client<%> sync-request] req))
]}

@defmethod[(adjust-request [req request?]) request?]{

Returns a request like @racket[req] except enriched with the client's header
fields and transformed by the client's request adjusters.

A header field from the client is only included if @racket[req] does not
already have a header field of the same name. That is, @racket[req]'s header
fields take precedence over the client's header fields.
}

@defmethod[(handle-response [resp (is-a?/c response<%>)])
           any]{

Notifies the client's response listeners (oldest first) and then handles the
response by calling the first matching response handler, or if none match, by
calling the default handler.

A handler entry matches @racket[resp] according to the following rules:
@itemlist[

@item{An entry of the form @racket[(list _status-code-integer _handler)] matches
if @racket[_status-code-integer] is equal to @racket[(send resp
@#,method[response<%> get-status-code])]. Examples of
@racket[_status-code-integer] include @racket[200] and @racket[404].}

@item{An entry of the form @racket[(list _status-class-symbol _handler)] matches
if @racket[_status-class-symbol] is equal to @racket[(send resp
@#,method[response<%> get-status-class])]. Examples of
@racket[_status-class-symbol] include @racket['successful] and
@racket['client-error].}

@item{An entry of the form @racket[(list 'else _handler)] always matches.}

]
If a handler is selected, it is called with @(this-obj) and @racket[resp].

The default handler calls @method[http-client<%> handle-response-content] if
@racket[resp] has the status code 200 (Found); otherwise, it closes
@racket[resp]'s content input port and raises an exception. (Closing the content
input port may cause an http/2 connection to cancel the corresponding stream.)
}

@defmethod[(handle-response-content [resp (is-a?/c response<%>)]) any]{

Handles the response by calling the first matching content handler on
@racket[resp]'s content input port, or if none match, by calling the default
content handler.

A content handler entry matches @racket[resp] according to the following rules:
@itemlist[

@item{An entry of the form @racket[(list _mime-type-symbol _handler)] matches if
@racket[_mime-type-symbol] is equal to @racket[(send resp
get-content-type)]. Examples of @racket[_mime-type-symbol] include
@racket['text/html] and @racket['application/json].}

@item{An entry of the form @racket[(list '*/* _handler)] always matches. (Note:
except for this specific case, a star in a @racket[_mime-type-symbol] is not
interpreted specially.)}

]
If a handler is selected, it is called with @racket[(send resp
@#,method[response<%> get-content-in] #t)] in a context where
@racket[current-response] is set to @racket[resp].

The default content handler closes @racket[resp]'s content input port and raises
an exception.
}

@defmethod[(sync-request [req request?]) (is-a?/c response<%>)]{

Sends a @tech{request} and returns the @tech{response}.

Equivalent to
@racket[((sync (send @#,(this-obj) @#,method[http-client<%> async-request] req)))].

This method does not use the client's response and content handlers.
}

@defmethod[(async-request [req request?]) (evt/c (-> (is-a?/c response<%>)))]{

Adjusts @racket[req] according to @method[http-client<%> adjust-request],
executes the request, and returns a synchronizable event that is ready once one
of the following occurs:
@itemlist[

@item{The beginning of a response has been received, including the status and
header. The synchronization result is a constant function that returns an
instance of @racket[response<%>]; see @method[response<%> get-content-in] for
notes on concurrent processing of the response message body.}

@item{The server closed the connection or sent an invalid response
beginning. The synchronization result is a function that raises an
exception with information about the failure.}

]

See @secref["evt-result"] for the rationale of the procedure wrapper.

An exception may be raised immediately if a failure occurs while sending the
request (for example, no connection could be made to the host).

This method does not use the client's response and content handlers.
}
}


@defparam[current-response resp (or/c #f (is-a?/c response<%>))]{

The @method[http-client<%> handle-response-content] sets this parameter to the
response being handled for the duration of a call to a content handler.
}

@defthing[response-handler/c contract?
          #:value (-> (is-a?/c http-client<%>) (is-a?/c response<%>) any)]{

Contract for response handlers. See @method[http-client<%> handle-response].
}

@defthing[content-handler/c contract?
          #:value (-> input-port? any)]{

Contract for content handlers. See @method[http-client<%>
handle-response-content].
}
