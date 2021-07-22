#lang scribble/manual
@(require (for-syntax racket/base)
          "util.rkt"
          (for-label racket/base racket/contract racket/class
                     net/url http123 http123/internal openssl))

@; ------------------------------------------------------------
@title[#:tag "internals"]{Internals}

@bold{Warning: } This section describes internal implementation details that may
change without warning. Contact the author before relying on this module.

@defmodule[http123/internal]

@; ----------------------------------------

@definterface[http-client-base<%> ()]{

Interface of objects that can implement the basic client operation of submitting
a request. The method @xmethod[http-client<%> async-request] is implemented by
forwarding it to an object implementing this interface (usually an instance of
@racket[http-connection-manager%]).

@defmethod[(async-request [req request?])
           (evt/c (-> (is-a?/c response<%>)))]{

Sends the request @racket[req] and returns an event that becomes ready when the
server either sends a response or when an error occurs. See also
@method[http-client<%> async-request].
}
}

@; ----------------------------------------
@; Connection Manager

@defclass[http-connection-manager% object% (http-client-base<%>)]{

Handles a request by creating connections (@racket[http-connection%]) based on
the request's location.

@defconstructor[((ssl (or/c 'secure 'auto ssl-client-context?) 'secure)
                 (custodian custodian? (current-custodian)))]

@defmethod[(get-connection [loc ok-http-url?])
           (is-a?/c http-connection%)]{

Returns the existing connection to the server specified by @racket[loc], or
creates a new connection using @method[http-connection-manager% open-connection]
if none exists yet. Connections are identified by the triple consisting of the
location's host, port, and scheme (@tt{https} vs @tt{http}).
}

@defmethod[(open-connection [host string?]
                            [port (integer-in 1 (sub1 (expt 2 16)))]
                            [https? boolean?])
           (is-a?/c http-connection%)]{

Creates a new connection to the server at @racket[host]:@racket[port], using
SSL/TLS if @racket[https?] is true.
}

@defmethod[(async-request [req request?])
           (evt/c (-> (is-a?/c response<%>)))]{

Equivalent to
@(let-syntax ([get-connection (method-name-tx #'http-connection-manager% #'get-connection)]
              [async-request (method-name-tx #'http-connection% #'async-request)])
@racketblock[
(send (send @#,(this-obj) get-connection (request-url req))
      async-request req)
])
}
}

@; ----------------------------------------
@; Connection

@defclass[http-connection% object% (http-client-base<%>)]{

Represents a connection to a specific server. A connection can be open or
closed; if a request is made to a closed connection, it is automatically
reopened by creating a new actual connection.

@defconstructor[((host string?)
                 (port (integer-in 1 (sub1 (expt 2 16))))
                 (ssl (or/c #f 'secure 'auto ssl-client-context?))
                 (protocols (listof symbol?) '(http/2 http/1.1))
                 (custodian custodian?))]

@defmethod[(get-actual-connection [connect? boolean? #t])
           http-actual-connection?]{

Gets the existing actual connection to the server. If none currently exists, one
is created using @method[http-connection% open-actual-connection] if
@racket[connect?] is true, or @racket[#f] is returned otherwise.
}

@defmethod[(open-actual-connection) (is-a?/c http-actual-connection<%>)]{

Creates a new actual connection to the server.
}

@defmethod[(make-http1-connection) (is-a?/c http-actual-connection<%>)]{

Creates an actual connection implementing the @(HTTP/1.1) protocol.
}

@defmethod[(make-http2-connection) (is-a?/c http-actual-connection<%>)]{

Creates an actual connection implementing the @(HTTP/2) protocol.
}

@defmethod[(close) void?]{

Closes the current actual connection, if one exists.
}

@defmethod[(async-request [req request?])
           (evt/c (-> (is-a?/c response<%>)))]{

Submits the request to @racket[(@#,method[http-connection%
get-actual-connection])] using @method[http-actual-connection<%>
open-request]. If the request submission fails, the current actual connection is
abandoned and the request is retried with a new actual connection. If it fails
too many times, an exception is raised.
}
}

@; ----------------------------------------
@; Actual connections

@definterface[http-actual-connection<%> ()]{

Represents an actual communication channel to a server.

@defmethod[(abandon) void?]{

Marks the actual connection as closed. It no longer accepts new requests, but it
continues to receive responses until the server hangs up.
}

@defmethod[(open-request [req request?])
           (or/c #f (evt/c (-> (is-a?/c response<%>))))]{

Submits the request to the server and returns an event, or returns @racket[#f]
if the submission failed (for example, if the connection was concurrently
closed).
}

@defmethod[(open?) boolean?]{

Returns @racket[#t] if the connection is still fully open---that is, new
requests can be sent to the server---or @racket[#f] if the connection is closed
or half-open.
}
}
