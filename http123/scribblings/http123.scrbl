#lang scribble/manual

@title{http123: HTTP Client}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@defmodule[http123]

This library implements an @as-index{HTTP client} with support for
both @as-index{HTTP/1.1} and @as-index{HTTP/2} protocols.

@; ------------------------------------------------------------

@bold{Development} Development of this library is hosted by
@hyperlink["http://github.com"]{GitHub} at the following project page:

@centered{@url{https://github.com/rmculpepper/http123}}

@bold{Copying} This library is licensed
@hyperlink["http://www.apache.org/licenses/LICENSE-2.0"]{Apache
License, Version 2.0}.

@; @(local-table-of-contents)

@; ------------------------------------------------------------

@include-section["intro.scrbl"]
@include-section["client-api.scrbl"]
@include-section["request.scrbl"]
@include-section["header.scrbl"]
@include-section["response.scrbl"]
@include-section["exn.scrbl"]
@include-section["util.scrbl"]
@include-section["notes.scrbl"]
@include-section["internal.scrbl"]
