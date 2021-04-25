#lang scribble/manual
@(require scribble/example
          racket/list
          racket/runtime-path
          "util.rkt"
          (for-label racket/base racket/contract racket/class
                     net/url-structs net/url-string
                     http123))

@title{http123: HTTP Client}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodule[http123]

Implements an @as-index{HTTP client} with support for both @as-index{http/1.1}
and @as-index{http/2} protocols.

@bold{Status: } The high-level client interface is unfinished. The http/1.1 and
http/2 protocol implementations are fairly complete, with some exceptions (see
@secref["known-issues"]).

@; ------------------------------------------------------------

@include-section["intro.scrbl"]
@include-section["client-api.scrbl"]
@include-section["base-api.scrbl"]
@include-section["exn.scrbl"]
@include-section["notes.scrbl"]
