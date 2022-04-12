;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base

;; ============================================================
;; Regexp

(module regexp racket/base
  (require scramble/regexp)
  (provide (all-defined-out))
  (define-syntax-rule (byte-px^$ part ...) (px #:byte ^ part ... $))
  (define-RE OWS #:byte (* (chars #\space #\tab)))
  (define-RE TCHAR #:byte (chars alpha digit "!#$&'*+-.^_`|~"))
  (define-RE TOKEN #:byte (+ TCHAR))
  (define-RE lower-TCHAR #:byte (chars (intersect TCHAR (complement upper))))
  (define-RE lower-TOKEN #:byte (+ lower-TCHAR)))

;; ============================================================
;; ASCII

(module ascii racket/base
  (provide (all-defined-out))

  (define (ascii-string? s)
    (and (string? s) (for/and ([c (in-string s)]) (< (char->integer c) 128))))
  (define (ascii-bytes? bs)
    (and (bytes? bs) (for/and ([b (in-bytes bs)]) (< b 128))))

  ;; Check after conversion to avoid race with concurrent modification.

  (define (string->bytes/ascii s [err-byte #f] [start 0] [end (string-length s)])
    (define bs (string->bytes/latin-1 s (or err-byte 255) start end))
    (unless (ascii-bytes? bs)
      (raise-argument-error 'string->bytes/ascii
                            "string cannot be encoded in ASCII" s))
    bs)

  (define (bytes->string/ascii bs [err-char #f] [start 0] [end (bytes-length bs)])
    (define err-char*
      (cond [(and err-char (< (char->integer err-char) 128)) err-char]
            [else (integer->char 255)]))
    (define s (bytes->string/latin-1 bs err-char* start end))
    (unless (ascii-string? s)
      (raise-argument-error 'bytes->string/ascii
                            "byte string is not a well-formed ASCII encoding" bs))
    s))

;; ============================================================
;; URLs

(module url racket/base
  (require racket/match
           net/uri-codec
           net/url-structs
           net/url-string
           scramble/regexp
           (only-in (submod ".." regexp) byte-px^$)
           (submod ".." ascii))
  (provide (all-defined-out))

  ;; Approximations
  (define-RE ip4-rx #:byte (cat (repeat (chars digit) 1 3) "." (repeat (chars digit) 3)))

  (define-RE h16 #:byte (repeat (chars xdigit) 1 4))
  (define-RE h16: #:byte (cat h16 ":"))
  (define-RE ls32 #:byte (or (cat h16 ":" h16) ip4-rx))

  ;; IPv6address   =                     6( h16: ) ls32
  ;;               /                "::" 5( h16: ) ls32
  ;;               / [   1( h16: ) ] ":" 4( h16: ) ls32
  ;;               / [ 1*2( h16: ) ] ":" 3( h16: ) ls32
  ;;               / [ 1*3( h16: ) ] ":" 2( h16: ) ls32
  ;;               / [ 1*4( h16: ) ] ":"    h16:   ls32
  ;;               / [ 1*5( h16: ) ] ":"           ls32
  ;;               / [ 1*6( h16: ) ] ":"           h16
  ;;               / [ 1*7( h16: ) ] ":"
  (define-RE ip6-exact-rx #:byte
    (or (cat                       (repeat h16: 6) ls32)
        (cat                  "::" (repeat h16: 5) ls32)
        (cat (repeat h16: 1)   ":" (repeat h16: 4) ls32)
        (cat (repeat h16: 1 2) ":" (repeat h16: 3) ls32)
        (cat (repeat h16: 1 3) ":" (repeat h16: 2) ls32)
        (cat (repeat h16: 1 4) ":" (repeat h16: 1) ls32)
        (cat (repeat h16: 1 5) ":"                 ls32)
        (cat (repeat h16: 1 6) ":" h16)
        (cat (repeat h16: 1 7) ":")))
  (define-RE ip6-approx-rx #:byte (cat (? ":") (repeat h16: 0 7) (or ls32 h16 ":")))
  (define-RE ip6-rx #:byte (cat "[" ip6-exact-rx "]"))

  (define-RE unreserved-rx #:byte (chars alpha digit "-._~"))
  (define-RE pct-encoded-rx #:byte (cat "%" (repeat (chars xdigit) 2)))
  (define-RE sub-delims-rx #:byte (chars "!$&'()*+,;="))
  (define-RE reg-name-rx #:byte (* (or unreserved-rx pct-encoded-rx sub-delims-rx)))

  (define-RE host-rx #:byte (or ip4-rx ip6-rx reg-name-rx))
  (define-RE host+port-rx #:byte (cat (report host-rx) ":" (report (+ (chars digit)))))

  (define (check-connect-target who loc)
    (define (bad msg loc)
      (error who "bad CONNECT target, ~a\n  target: ~e" msg loc))
    (define (bad-expect loc)
      (bad "expected byte string containing host:port" loc))
    (define loc-bytes
      (cond [(bytes? loc) (bytes->immutable-bytes loc)]
            [else (bad-expect loc)]))
    (match (parse-connect-target loc-bytes)
      [(list host port)
       (unless (< 0 port (expt 2 16)) (bad "port number out of range" loc-bytes))
       loc-bytes]
      [_ (bad-expect loc-bytes)]))

  ;; parse-connect-target : Bytes -> (list Bytes Nat) or #f
  (define (parse-connect-target loc)
    (match (regexp-match (byte-px^$ host+port-rx) loc)
      [(list _ host port-bs)
       (list host (string->number (bytes->string/latin-1 port-bs)))]
      [else #f]))

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
       (define enc-host (uri-encode host))
       (string->immutable-string
        (cond [(or (not port) (equal? port (scheme-default-port scheme))) enc-host]
              [else (format "~a:~a" enc-host port)]))]))

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
    (string->bytes/ascii (url->string u))))

;; ============================================================
;; Ports

(module port racket/base
  (require racket/match
           racket/tcp
           openssl
           scramble/evt)
  (provide (all-defined-out))

  ;; ----------------------------------------
  ;; Abandon

  ;; Warning: this might interact strangely with finalization...
  (define abandon-table (make-weak-hasheq))

  ;; Abandonability is not automatically propagated through
  ;; prop:input-port and prop:output-port.

  (define (port-with-abandon? v)
    (or (tcp-port? v) (ssl-port? v) (hash-has-key? abandon-table v)))

  (define (abandon-port p [or-close? #t])
    (cond [(tcp-port? p) (tcp-abandon-port p)]
          [(ssl-port? p) (ssl-abandon-port p)]
          [(hash-ref abandon-table p #f)
           => (lambda (abandon) (abandon p))]
          [or-close?
           (when (output-port? p) (close-output-port p))
           (when (input-port? p) (close-input-port p))]
          [else (error 'abandon-port "cannot abandon port: ~e" p)]))

  ;; ----------------------------------------
  ;; Constructors

  (define (make-input-port* #:name name
                            #:read-in read-in
                            #:peek peek
                            #:close close
                            #:abandon [abandon #f]
                            #:get-progress-evt [get-progress-evt #f]
                            #:commit [commit #f]
                            #:get-location [get-location #f]
                            #:count-lines! [count-lines! void]
                            #:init-position [init-position 1]
                            #:buffer-mode [buffer-mode #f])
    (define (make close)
      (make-input-port name read-in peek close
                       get-progress-evt commit get-location
                       count-lines! init-position buffer-mode))
    (cond [(procedure? abandon)
           (define abandon-cell (make-thread-cell #f #f))
           (define (close/check-for-abandon)
             (if (thread-cell-ref abandon-cell)
                 (abandon)
                 (close)))
           (define port (make close/check-for-abandon))
           (define (do-abandon p)
             (thread-cell-set! abandon-cell #t) (close-input-port p))
           (hash-set! abandon-table port do-abandon)]
          [else (make close)]))

  (define (make-output-port* #:name name
                             #:evt evt
                             #:write-out write-out
                             #:close close
                             #:abandon [abandon #f]
                             #:write-out-special [write-out-special #f]
                             #:get-write-evt [get-write-evt #f]
                             #:get-write-special-evt [get-write-special-evt #f]
                             #:get-location [get-location #f]
                             #:count-lines! [count-lines! void]
                             #:init-position [init-position 1]
                             #:buffer-mode [buffer-mode #f])
    (define (make close)
      (make-output-port name evt write-out close
                        write-out-special get-write-evt get-write-special-evt
                        get-location count-lines! init-position buffer-mode))
    (cond [(procedure? abandon)
           (define abandon-cell (make-thread-cell #f #f))
           (define (close/check-for-abandon)
             (if (thread-cell-ref abandon-cell)
                 (abandon)
                 (close)))
           (define port (make close/check-for-abandon))
           (define (do-abandon p)
             (thread-cell-set! abandon-cell #t) (close-output-port p))
           (hash-set! abandon-table port do-abandon)]
          [else (make close)]))

  ;; ----------------------------------------
  ;; Exn-propagating ports

  ;; wrap-input-port : InputPort -> (values InputPort (-> Exn Void))
  ;; FIXME: should reader receive exn immediately or in place of EOF?
  (define (wrap-input-port in [name (object-name in)]
                           #:delay-to-eof? [delay-to-eof? #f])
    (define exnbe (make-box-evt))
    (define exn-evt (wrap-evt exnbe raise))
    ;; FIXME: could implement check directly, instead of via sync/timeout
    (define (check) (when (box-evt-ready? exnbe) (sync/timeout 0 exn-evt)))
    (define (check-early) (unless delay-to-eof? (check)))
    (define (check-at-eof) (check))
    (define (read-in buf)
      (check-early)
      (define r (read-bytes-avail!* buf in))
      (cond [(eqv? r 0)
             (choice-evt (handle-evt in (lambda (_in) (read-in buf)))
                         exn-evt)]
            [(eof-object? r) (check-at-eof) r]
            [else r]))
    (define (peek buf skip progress-evt)
      (check-early)
      (define r (peek-bytes-avail!* buf skip progress-evt in))
      (cond [(eqv? r 0)
             (choice-evt (handle-evt in (lambda (_e) (peek buf skip progress-evt)))
                         (handle-evt (or progress-evt never-evt) (lambda (_e) #f))
                         exn-evt)]
            [(eof-object? r) (check-at-eof) r]
            [else r]))
    (define (close) (close-input-port in))
    (define (abandon) (abandon-port in))
    (define get-progress-evt
      (and (port-provides-progress-evts? in)
           (lambda () (port-progress-evt in))))
    (define (commit k progress-evt done)
      (check-early)
      (port-commit-peeked k progress-evt done in))
    (define (get-location)
      (port-next-location in))
    (define (count-lines!)
      (port-count-lines! in))
    (values
     (make-input-port* #:name name
                       #:read-in read-in
                       #:peek peek
                       #:close close
                       #:abandon (and (port-with-abandon? in) abandon)
                       #:get-progress-evt get-progress-evt
                       #:commit commit
                       #:get-location get-location
                       #:count-lines! count-lines!)
     (lambda (e) (box-evt-set! exnbe e))))

  (define (make-wrapped-pipe)
    (define-values (in out) (make-pipe))
    (define-values (wrapped-in raise-exn) (wrap-input-port in 'wrapped-pipe))
    (values wrapped-in out raise-exn))

  ;; ----------------------------------------
  ;; Buffering

  ;; SSL output ports currently seem to create one SSL record per port write.
  ;; This causes an explosion in the amount of data sent, due to padding, HMAC,
  ;; and other record overhead. So add an ad hoc buffering port around it.

  ;; buffering-output-port : OutputPort Boolean [PosInt] -> OutputPort
  ;; The buffer is unlimited. It is flushed in increments of at most MAX-CHUNK.
  ;; This wrapper should be thread-safe and break-safe, but it is not kill-safe.
  (define (buffering-output-port out propagate-close?
                                 [max-flush-chunk-length 4096])
    (define-values (buf-in buf-out) (make-pipe))
    (define flush-lock (make-semaphore 1))
    (define (write-out buf start end non-block? eb?)
      (cond [(< start end)
             (cond [non-block?
                    ;; must not buffer, must not block, eb? = #f
                    (define (retry _) (write-out buf start end #t #f))
                    (cond [(zero? (pipe-content-length buf-in))
                           ;; buffer is empty => write directly to out
                           (write-direct buf start end)]
                          [else ;; buffer is non-empty => try to flush first
                           (cond [(not (semaphore-try-wait? flush-lock))
                                  (handle-evt (semaphore-peek-evt flush-lock) retry)]
                                 [else ;; acquired flush-lock
                                  (if (try-flush-without-block)
                                      (write-direct buf start end)
                                      (begin (semaphore-post flush-lock)
                                             (handle-evt out retry)))])])]
                   [else ;; can buffer, can block
                    buf-out])]
            [else ;; flush => non-block? = #f (allowed to block)
             (begin (flush eb?) 0)]))
    (define (write-direct buf start end) ;; non-block? = #t, eb? = #f
      (or (write-bytes-avail* buf out start end)
          (handle-evt out (lambda (_) (write-out buf start end #t #f)))))
    (define (try-flush-without-block) ;; returns #t if flush completed
      ;; PRE: holding flush-lock, breaks disabled
      (define buffered (pipe-content-length buf-in))
      (cond [(zero? buffered)
             #t]
            [else
             (define len (min buffered (or max-flush-chunk-length buffered)))
             (define progress-evt (port-progress-evt buf-in))
             (define chunk (peek-bytes len buf-in))
             (define r (write-bytes-avail* chunk out))
             (cond [(or (eq? r #f) (zero? r))
                    #f]
                   [else
                    (port-commit-peeked r progress-evt always-evt buf-in)
                    (try-flush-without-block)])]))
    (define (flush/can-block eb?) ;; PRE: holding flush-lock
      (cond [(try-flush-without-block) (void)]
            [else (begin (if eb? (sync/enable-break out) (sync out)) (flush/can-block eb?))]))
    (define (flush eb?)
      (parameterize-break #f
        (if eb? (semaphore-wait/enable-break flush-lock) (semaphore-wait flush-lock))
        (flush/can-block eb?)
        (semaphore-post flush-lock)))
    (define (close)
      (flush #t)
      (when propagate-close? (close-output-port out)))
    (define (abandon)
      (flush #t)
      (when propagate-close? (abandon-port out)))
    (define (get-write-evt buf start end)
      (guard-evt
       (lambda ()
         (let retry ()
           (cond [(not (semaphore-try-wait? flush-lock))
                  (replace-evt (semaphore-peek-evt flush-lock)
                               (lambda (_) (retry)))]
                 [else ;; acquired flush-lock
                  (cond [(parameterize-break #f
                           (begin0 (try-flush-without-block)
                             (semaphore-post flush-lock)))
                         ;; buffer is currently empty
                         (write-bytes-avail-evt buf out start end)]
                        [else (replace-evt out (lambda (_) (retry)))])])))))
    (make-output-port* #:name (object-name out)
                       #:evt always-evt
                       #:write-out write-out
                       #:get-write-evt (and (port-writes-atomic? out)
                                            get-write-evt)
                       #:close close
                       #:abandon (and propagate-close?  ;; otherwise, no point in abandon
                                      (port-with-abandon? out)
                                      abandon)))

  ;; ----------------------------------------
  ;; Proxy

  ;; A proxy output port writes to the underlying port, but closing it
  ;; removes the capability without closing the underlying port.

  (define (proxy-output-port out)
    (make-output-port* #:name (object-name out)
                       #:evt out
                       #:write-out out
                       #:close void
                       #:get-write-evt (and (port-writes-atomic? out)
                                            (lambda (buf start end)
                                              (write-bytes-avail-evt buf out start end))))))
