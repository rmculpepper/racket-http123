#lang scribble/manual
@(require scribble/example
          racket/runtime-path
          "util.rkt"
          (for-label racket/base racket/contract racket/class
                     net/url http123))

@(begin
  (define-runtime-path log-file "log-header.rktd")
  (define the-eval (make-log-based-eval log-file 'record))
  (the-eval '(require http123 racket/class racket/port racket/pretty
                      (submod http123/scribblings/util pretty))))

@; ------------------------------------------------------------
@title[#:tag "header"]{Headers}

A request or response contains a single @emph{header}, which consists
of zero or more @emph{header fields}. Each field has a @emph{name} (or
@emph{key}) and a @emph{value}.


@; ------------------------------------------------------------
@section[#:tag "header-fields"]{Header Fields}

Requests use the following representation of headers: a header is a
list of header fields. The user may supply header fields in the form
described by @racket[in-header-field/c], but they are checked and
converted into the form described by @racket[header-field/c].

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


@; ------------------------------------------------------------
@section[#:tag "header-obj"]{Header Objects}

A @racket[header<%>] instance contains a list of header fields and
adds useful methods.

@definterface[header<%> ()]{

Interface for objects representing @rfc7231["section-7"]{response headers} and
@h11rfc["section-4.1.2"]{trailers}.

@defmethod[(get-header-field-list) (listof header-field/c)]{

Gets a list of header fields, where each entry has the form @racket[(list
_key-bytes _value-bytes)]. The ordering of different keys in the resulting list
is not specified.

If a given @racket[_key] has multiple values, the result list has multiple
entries for that key---that is, values are not combined. The result list
preserves the order of values for a given key.
}

@defmethod[(get-header-lines) (listof bytes?)]{

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
