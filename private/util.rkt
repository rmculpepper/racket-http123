;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/match
         net/url-structs
         net/url-string)
(provide (all-defined-out))

;; ============================================================
;; ASCII

(define (ascii-string? s)
  (and (string? s) (for/and ([c (in-string s)]) (< (char->integer c) 128))))
(define (ascii-bytes? bs)
  (and (bytes? bs) (for/and ([b (in-bytes bs)]) (< b 128))))

;; Check after conversion to avoid race with concurrent modification.

(define (string->bytes/ascii s [err-byte #f] [start 0] [end (string-length s)])
  (define bs (string->bytes/latin-1 s 255 start end))
  (unless (ascii-bytes? bs)
    (raise-argument-error 'string->bytes/ascii
                          "string cannot be encoded in ASCII" s))
  bs)

(define (bytes->string/ascii bs [err-char #f] [start 0] [end (bytes-length bs)])
  (define s (bytes->string/latin-1 bs err-char start end))
  (unless (ascii-string? s)
    (raise-argument-error 'bytes->string/ascii
                          "byte string contains non-ASCII characters" bs))
  s)


;; ============================================================
;; URLs

(define (check-http-url who loc [orig loc])
  (match loc
    [(url scheme user host port path-abs? path query fragment)
     (define (bad msg) (error who "~a\n  URL: ~e" msg orig))
     (unless scheme (bad "bad URL, missing scheme"))
     (define scheme* (normalize-http-scheme scheme))
     (unless scheme* (bad "bad URL, expected \"http\" or \"https\" for scheme"))
     (when user (bad "bad URL, contains userinfo"))
     (unless host (bad "bad URL, missing host"))
     (define host* (string->immutable-string (string-downcase host)))
     (unless path-abs? (bad "bad URL, path is not absolute"))
     (cond [(and (eq? scheme* scheme) (eq? host* host)) loc]
           [else (url scheme* #f host* port path-abs? path query fragment)])]
    [(? string?)
     (define u
       (with-handlers ([exn:fail?
                        (lambda (e)
                          (error who "malformed URL string;\n ~a" (exn-message e)))])
         (string->url loc)))
     (check-http-url who u orig)]
    [_ (error who "expected string or URL\n  given: ~e" orig)]))

(define (normalize-http-scheme scheme) ;; if good, result is immutable, downcased
  (cond [(string-ci=? scheme "https") "https"]
        [(string-ci=? scheme "http") "http"]
        [else #f]))

(define (ok-http-url? u)
  (match u
    [(url (? string? scheme) #f (? string? host) port #t path query fragment)
     (and (normalize-http-scheme scheme) #t)]
    [_ #f]))

;; RFC 7230, 5.3
(define (url-origin-form u) ;; absolute-path ["?" query]
  (match u
    [(url scheme user host port #t path  query fragment)
     (define path* (if (null? path) (list (path/param "" '())) path))
     (url #f     #f   #f   #f   #t path* query #f)]))
(define (url-absolute-form u) ;; absolute-URI from RFC 3986, 4.3
  (match u
    [(url scheme user host port #t path query fragment)
     (url scheme user host port #t path query #f)]))

;; RFC 7230, 5.4
(define (url->host-string u)
  (match u
    [(url scheme user host port #t path query fragment)
     (string->immutable-string
      (cond [(or (not port) (equal? port (scheme-default-port scheme))) host]
            [else (format "~a:~a" host port)]))]))

(define (url->host-bytes u)
  (string->bytes/ascii (url->host-string u)))

(define (scheme-default-port scheme)
  (cond [(string-ci=? scheme "http") 80]
        [(string-ci=? scheme "https") 443]
        [else #f]))

;; ----

(define (url->origin-form-bytes u)
  (url->bytes (url-origin-form u)))
(define (url->absolute-form-bytes u)
  (url->bytes (url-absolute-form u)))

(define (url->bytes u)
  (string->bytes/ascii (url->string u)))

;; ----

(module pretty racket/base
  (require racket/serialize)
  (serializable-struct pretty (s)
    #:property prop:custom-write
    (lambda (self out mode) (write-string (pretty-s self) out)))
  (provide (all-defined-out)))
