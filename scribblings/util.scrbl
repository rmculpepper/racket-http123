#lang scribble/manual
@(require scribble/example
          racket/runtime-path
          "util.rkt"
          (for-label racket/base racket/contract racket/class net/uri-codec json
                     net/url http123 http123/util/url http123/util/header http123/util/request))

@(begin
  (define the-eval (make-base-eval))
  (the-eval '(require http123 racket/class racket/port racket/pretty net/url
                      http123/util/url http123/util/header http123/util/request
                      (submod http123/scribblings/util pretty))))

@; ------------------------------------------------------------
@title[#:tag "util"]{Utilities}


@; ------------------------------------------------------------
@section[#:tag "url-util"]{URL Utilities}

@defmodule[http123/util/url]

@defproc[(build-url [base-url (or/c url? string?)]
                    [path-part (or/c string? path/param?)] ...
                    [#:query query (listof (cons/c symbol? (or/c #f string?))) null])
         url?]{

Builds a new @racket[url] structure by extending @racket[base-url] with the
given @racket[path-part]s and @racket[query] parameters.

Each @racket[path-part] is added to the end of @racket[base]'s existing path. If
@racket[path-part] is a string, it is first split on @litchar{/} characters,
@racket["."] and @racket[".."] segments are converted to @racket['same] and
@racket['up], respectively, and then each segment is wrapped in a
@racket[path/param] with no parameters. Note that the @racket[path-part] strings
are not decoded---for example, the @racket[path-part] value @racket["%2F"] does
not represent a one-character segment; it represents a three-character segment
that gets encoded as @litchar{%252F} when the URL is converted to a string.

The path is then simplified as follows: @racket['up] and @racket['same] segments
are collapsed unless doing so would delete parameters, and empty path segments
are removed except that an empty segment is allowed at the end of the path. (An
empty segment at the end corresponds to a URL written with a final @litchar{/}.)

The @racket[query] is appended to the end of @racket[base]'s existing query arguments.
}

@defproc[(ok-http-url? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a URL structure (@racket[url?]) that
satisfies the following constraints, @racket[#f] otherwise. The constraints are:
@itemlist[

@item{the scheme is @racket["http"] or @racket["https"] (compared case-insensitively)}
@item{the user field is absent (@racket[#f])}
@item{the host field is present}
@item{the path is marked as absolute}

]

@examples[#:eval the-eval
(ok-http-url? (string->url "http://racket-lang.org/"))
(ok-http-url? (string->url "http://ryanc@racket-lang.org/"))
(ok-http-url? (string->url "ftp://mirror.racket-lang.org/"))
(ok-http-url? (string->url "somewhere/out-there.html"))
]}


@; ------------------------------------------------------------
@section[#:tag "header-util"]{Header Utilities}

@defmodule[http123/util/header]

@defthing[in-header-field/c contract?
          #:value (or/c string?
                        bytes?
                        (list/c (or/c symbol? string? bytes?)
                                (or/c string? bytes?)))]{

Represents the forms that a user may supply a header field as an
argument (for example, the header argument to @racket[request]).

Examples:
@racketblock[
"Accept-Encoding:   gzip, deflate  "
'(accept-language #"en")
'(#"User-Agent" #"racket-http123/0.1")
]}

@defthing[header-field/c contract?
          #:value (list/c (and/c bytes? immutable? ...lowercase-token-rx...)
                          (and/c bytes? immutable? ...field-value-rx...))]{

The canonical representation of a header field. The header field name
is represented as an immutable byte string with no upper-case
letters. The header field value is represented as an immutable byte
string; whitespace is trimmed from the beginning and end of the field
value.

Examples:
@racketblock[
'(#"accept-encoding" #"gzip, deflate")
'(#"accept-language" #"en")
'(#"user-agent" #"racket-http123/0.1")
]}


@defproc[(check-header-field [in in-header-field/c])
         header-field/c]{


}

@defproc[(header-field-list-update [base-header (listof header-field/c)]
                                   [ext-header (listof header-field/c)])
         (listof header-field/c)]{


}


@; ------------------------------------------------------------
@section[#:tag "request-util"]{Request Utilities}

@defmodule[http123/util/request]

@defthing[method/c contract?
          #:value (or/c 'GET 'HEAD 'POST 'PUT 'DELETE 'OPTIONS 'TRACE 'PATCH)]{

Contract for HTTP methods.
}

@defproc[(request/json [method method/c]
                       [url (or/c string? url?)]
                       [header (listof in-header-field/c)]
                       [json-data jsexpr?]
                       [#:write? write? boolean? #f])
           request?]{

Creates a @tech{request} whose body is the JSON represented by
@racket[json-data]. A header field of the form @tt{Content-Type:
application/json} is automatically added to @racket[header].

If @racket[write?] is false, then @racket[json-data] is immediately converted to
a byte string. Otherwise, the request contains a procedure that writes
@racket[json-data] on demand.
}

@defproc[(request/form-urlencoded [method method/c]
                                  [url (or/c string? url?)]
                                  [header (listof in-header-field/c)]
                                  [form-data (listof (cons/c symbol? (or/c #f string?)))])
           request?]{

Creates a @tech{request} whose body is the encoding of @racket[form-data] using
@racket[alist->form-urlencoded]. A header field of the form @tt{Content-Type:
application/x-www-form-urlencoded} is automatically added @racket[header].
}
        
@defproc[(request/multipart [method method/c]
                            [url (or/c string? url?)]
                            [header (listof in-header-field/c)]
                            [data (listof (let* ([name/c (or/c string? symbol? bytes?)]
                                                 [value/c (or/c string? bytes? procedure?)]
                                                 [in-header/c (listof in-header-field/c)])
                                            (or/c (list/c name/c value/c)
                                                  (list/c name/c value/c in-header/c)
                                                  (list/c name/c value/c in-header/c '#:filename name/c))))])
           request?]{

Creates a @tech{request} whose body is the encoding of @racket[data] following
the @hyperlink["https://tools.ietf.org/html/rfc7578"]{multipart/form-data}
encoding rules. A header field of the form @tt{Content-Type:
multipart/form-data} with a randomly-generated boundary is automatically added
to @racket[header].

The @racket[data] consists of a list of parts. Each part has one of the
following forms:
@itemlist[

@item{@racket[(list _name _value)] --- Consists of a form field name and its
corresponding value. Example: @racket[(list "language" "en")].}

@item{@racket[(list _name _value _header)] --- Like the previous form, but also
includes a header that describes this field. Example: @racket[(list "subtotal"
"€20" '("content-type: text/plain;charset=utf-8"))]}

@item{@racket[(list _name _value _header '#:filename _filename)] --- Like the
previous form, but indicates that the part corresponds to a file with the given
@racket[_filename].}

]}


@; ----------------------------------------
@(close-eval the-eval)
