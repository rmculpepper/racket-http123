#lang scribble/manual
@(require scribble/example
          racket/runtime-path
          "util.rkt"
          (for-label racket/base racket/contract racket/class
                     net/url http123 http123/util/header))

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


@; ------------------------------------------------------------
@section[#:tag "header-obj"]{Header Objects}

A @racket[header<%>] instance contains a list of header fields and
adds useful methods.

@definterface[header<%> ()]{

Interface for objects representing @rfc7231["section-7"]{response headers} and
@h11rfc["section-4.1.2"]{trailers}.

@defmethod[(get-header-fields) (listof header-field/c)]{

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

@defmethod[(has-key? [key header-field-key?]) boolean?]{

Returns @racket[#t] if the header contains a field named @racket[key],
@racket[#f] otherwise.
}

@defmethod[(get-values [key header-field-key?]) (or/c (listof bytes?) #f)]{

Returns the list of field values associated with @racket[key], or @racket[#f] if
the header does not contain a field named @racket[key].

The result list contains one element per field line in the original header. That
is, this method does not perform splitting or combination of field values.
}

@defmethod[(get-value [key header-field-key?]) (or/c bytes? #f)]{

Returns the value associated with @racket[key] as a single byte string, or
@racket[#f] if the header does not contain a field named @racket[key].

If the original header contained multiple lines for @racket[key], the values are
combined by concatenating them separated by @racket[#", "]. Use
@method[header<%> get-values] instead for fields that do not have
comma-separable values, such as @tt{Set-Cookie}.
}

@defmethod[(get-integer-value [key header-field-key?]) (or/c exact-integer? #f)]{

Returns the integer value associated with @racket[key]. If @racket[key] has no
value, or if the value is not a single exact integer, returns @racket[#f]
instead.
}
}

@defproc[(header-field-key? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a byte string that is a valid,
canonical header field name, @racket[#f] otherwise.

A valid header field name must match the grammar for
@h11rfc["appendix-B"]{token}; in addition, it must not contain any
upper-case letters.

@examples[#:eval the-eval
(header-field-key? #"content-length")
(header-field-key? #"Content-Length")
]}

@; ------------------------------------------------------------
@(close-eval the-eval)
