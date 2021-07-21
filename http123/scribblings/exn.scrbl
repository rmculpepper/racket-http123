#lang scribble/manual
@(require "util.rkt"
          (for-label racket/base racket/contract racket/class
                     http123))

@title[#:tag "exn"]{Exceptions}

@defstruct*[(exn:fail:http123 exn:fail) ([info (hash/c symbol? any/c)])]{

Represents an error related to this library, including protocol errors,
communication errors, and misues of library features.

The @racket[info] field contains an immutable hash with additional details about
the error. The following are some common keys in @racket[info]; the presence or
absence of any of these keys depends on the specific error.
@itemlist[

@item{@racket['code] --- a symbol indicating what kind of error occurred, in a
form more suitable for comparison than parsing the error message}

@item{@racket['version] --- either @racket['http/1.1] or @racket['http/2], only
present for protocol-dependent errors}

@item{@racket['http2-error] --- a symbol indicating an http/2
@h2rfc["section-7"]{error code} (eg, @racket['PROTOCOL_ERROR]) or
@racket['unknown] if the error code is unfamiliar, only present when using
http/2}

@item{@racket['received] --- one of @racket['yes], @racket['no], or
@racket['unknown], indicating whether the request was received and processed by
the server}

@item{@racket['request] --- the @tech{request} that the error is related to}

@item{@racket['response] --- the @tech{response} that the error is related to}

@item{@racket['wrapped-exn] --- contains a more specific exception that
represents the immediate source of the error}

]}
