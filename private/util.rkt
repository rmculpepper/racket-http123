#lang racket/base
(require racket/contract/base
         racket/match
         net/url-structs
         net/url-string)
(provide ascii-string?
         ascii-bytes?
         (contract-out
          #:unprotected-submodule unchecked
          [string->bytes/ascii
           (->* [string?]
                [(or/c #f byte?) exact-nonnegative-integer? exact-nonnegative-integer?]
                bytes?)]
          [bytes->string/ascii
           (->* [bytes?]
                [(or/c #f char?) exact-nonnegative-integer? exact-nonnegative-integer?]
                string?)]
          [url->bytes
           (-> url? ascii-bytes?)]))


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

(define (check-http-url who u)
  (define (bad fmt . args) (apply error who fmt args))
  (match-define (url scheme user host port path-abs? path query fragment) u)
  (unless scheme (bad "URL missing scheme"))
  (unless (or (string-ci=? scheme "http") (string-ci=? scheme "https"))
    (bad "URL scheme is not \"http\" or \"https\""))
  (when user (bad "URL contains userinfo"))
  (unless host (bad "URL missing host"))
  (unless path-abs? (bad "URL path is not absolute"))
  ;; just silently ignore fragment
  (void))

(define (ok-http-url? u)
  (match u
    [(url scheme user host port path-abs? path query fragment)
     (and (string? scheme)
          (or (string-ci=? scheme "http") (string-ci=? scheme "https"))
          (not user)
          path-abs?)]
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
