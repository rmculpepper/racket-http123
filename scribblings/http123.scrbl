#lang scribble/manual

@title{http123: HTTP Client}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodule[http123]

Implements an @as-index{HTTP client} with support for both @as-index{http/1.1}
and @as-index{http/2} protocols.

@; ------------------------------------------------------------

@include-section["intro.scrbl"]
@include-section["client-api.scrbl"]
@include-section["request.scrbl"]
@include-section["header.scrbl"]
@include-section["response.scrbl"]
@include-section["exn.scrbl"]
@include-section["util.scrbl"]
@include-section["notes.scrbl"]
