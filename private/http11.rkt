#lang racket/base
(require racket/class
         racket/match
         racket/list
         racket/string
         racket/port
         racket/tcp
         net/url-structs
         net/url-string
         binaryio/reader
         openssl
         "interfaces.rkt"
         "header.rkt"
         "regexp.rkt"
         "io.rkt"
         file/gunzip)
(provide (all-defined-out))

;; References:
;; - HTTP/1.1: https://tools.ietf.org/html/rfc7230

(define default-user-agent-header
  (format "User-Agent: Racket/~a (http123)" (version)))

(define PIPE-SIZE 4096)

(define http11-connection%
  (class* object% (#; http-connection<%>)
    (init-field host
                port
                ssl)
    (super-new)

    (define/public (get-host) host)
    (define/public (get-port) port)

    ;; FIXME: need marker for connections that always error (eg, not an HTTP server)
    (define conn #f)

    (define/public (get-actual-connection)
      (cond [conn
             conn]
            [else
             (define c (open-actual-connection))
             (set! conn c)
             c]))

    (define/public (open-actual-connection)
      (define-values (in out)
        (cond [ssl (ssl-connect host port ssl)]
              [else (tcp-connect host port)]))
      (new http11-actual-connection%
           (parent this) (in in) (out out)))

    (define/public (close)
      (when conn
        (define c conn)
        (set! conn #f)
        (send c close)))

    (define/public (url->host-bytes u)
      (define scheme (url-scheme u))
      (define host (or (url-host u) (get-host)))
      (define port (or (url-port u) (get-port)))
      (string->bytes/utf-8
       (cond [(= port (case scheme [("http") 80] [("https") 443] [else #f])) host]
             [else (format "~a:~a" host port)])))

    ))

(define STATUS-EOL-MODE 'return-linefeed)
(define HEADER-EOL-MODE 'return-linefeed)
(define CHUNKED-EOL-MODE 'return-linefeed)
(define CONTENT-LENGTH-READ-NOW (expt 2 20)) ;; FIXME

(define-rx STATUS-CODE #px#"[0-9]{3}")
(define-rx STATUS-LINE
  (rx ^ (record "HTTP/[0-9.]+") " " (record STATUS-CODE) " " (record ".*") $))

;; represents an actual connection, without reconnect ability
(define http11-actual-connection%
  (class* object% (#; http-connection<%>)
    (init-field parent in out)
    (super-new)

    (define/public (live?)
      (and in out (not (port-closed? in)) (not (port-closed? out))))

    (define/public (close)
      (when out (close-output-port out) (set! out #f))
      (when in (close-input-port in) (set! in #f)))

    (define/public (abandon)
      ;; Only abandon output port.
      (when out (abandon-port out)))

    (define/private (abandon-port p)
      (cond [(tcp-port? p) (tcp-abandon-port p)]
            [(ssl-port? p) (ssl-abandon-port p)]
            [else (internal-error "cannot abandon port: ~e" p)]))

    ;; ----------------------------------------
    ;; Request and Response

    (define/public (sendrecv #:method method
                             #:url u
                             #:headers [headers null]
                             #:data [data #f]
                             #:close? [close? #f]
                             #:decodes [decodes '(gzip deflate)])
      (send-request #:method method
                    #:url u
                    #:headers headers
                    #:data data
                    #:close? close?
                    #:decodes decodes)
      (read-response #:method method
                     #:close? close?
                     #:decodes decodes))

    ;; ----------------------------------------
    ;; Request (Output)

    (define/public (send-request #:method method            ; Symbol, eg 'GET
                                 #:url u                    ; URL
                                 #:headers [headers null]   ; (Listof Bytes)
                                 #:data [data #f]           ; (U Bytes (Bytes -> Void) #f)
                                 #:close? [close? #f]       ; Boolean
                                 #:decodes [decodes '(gzip deflate)]) ; (Listof Symbol)
      ;; FIXME: check not closed/abandoned?
      (define url-bs (url->bytes u))
      (define host-bs (send parent url->host-bytes u))
      (fprintf out "~a ~a HTTP/1.1\r\n" method url-bs)
      (define more-headers
        (list (and (headerset-missing? headers #rx"^(?i:Host:) +.+$")
                   ;; RFC 7230 Section 5.4 (Host): The Host header is required,
                   ;; and it must be the same as the authority component of the
                   ;; target URI (minus userinfo).
                   ;; FIXME
                   (format "Host: ~a" host-bs))
              (and (pair? decodes)
                   (headerset-missing? headers #rx"^(?i:Accept-Encoding:) +.+$")
                   (format "Accept-Encoding: ~a"
                           (string-join (map symbol->string decodes) ",")))
              (and (headerset-missing? headers #rx"^(?i:User-Agent:) +.+$")
                   default-user-agent-header)
              (cond [(procedure? data)
                     (format "Transfer-Encoding: chunked")]
                    [(and (bytes? data)
                          (headerset-missing? headers #rx"^(?i:Content-Length:) +.+$"))
                     (format "Content-Length: ~a" (bytes-length data))]
                    [else
                     ;; If no content data, don't add Content-Length header.
                     #f])
              (and (and close? (headerset-missing? headers #rx"^(?i:Connection:) +.+$"))
                   (format "Connection: close\r\n"))))
      (for ([h (in-list more-headers)] #:when h)
        (fprintf out "~a\r\n" h))
      (for ([h (in-list headers)])
        (fprintf out "~a\r\n" h))
      (fprintf out "\r\n")
      (cond [(procedure? data)
             (let ([out out])
               (call-with-continuation-barrier
                (lambda ()
                  (data (λ (bs)
                          (define len (bytes-length bs))
                          (unless (zero? len)
                            (fprintf out "~x\r\n~a\r\n" len bs))))
                  (set! out #f))))
             (fprintf out "0\r\n\r\n")]
            [(bytes? data)
             (write-bytes data out)]
            [else (void)])
      (flush-output out))

    ;; ----------------------------------------
    ;; Response (Input)

    (define in-sema (make-semaphore 1))
    (define br
      (make-binary-reader in
        #:error-handler
        (make-binary-reader-error-handler
         #:error (lambda (br who fmt . args) (comm-error fmt . args))
         #:show-data? (lambda (br who) #f))))

    (define/private (acquire-input-mutex) (semaphore-wait in-sema))
    (define/private (release-input-mutex) (semaphore-post in-sema))
    (define/private (input-mutex-held?)
      (not (sync/timeout 0 (semaphore-peek-evt in-sema))))

    ;; Note: This *does not* release the input mutex when the procedure returns,
    ;; unless there is an error (and then it closes the connection).
    (define/private (call/acquire-input-mutex proc)
      (acquire-input-mutex)
      (with-handlers ([(lambda (e) #t)
                       (lambda (e)
                         (when (input-mutex-held?)
                           (close)
                           (release-input-mutex))
                         (raise e))])
        (call-with-continuation-barrier proc)))

    (define/private (comm-error fmt . args)
      (close)
      (release-input-mutex)
      (apply error* fmt args))

    (define/public (read-response #:method method
                                  #:close? [iclose? #f]
                                  #:decodes [decodes '(gzip deflate)])
      (call/acquire-input-mutex
       (lambda ()
         (define-values (status-line http-version status-code) (read-status-line))
         (define raw-headers (read-raw-headers))
         (define headers (new headers% (raw-headers raw-headers)))
         (define no-content? ;; RFC 7230 Section 3.3.3, cases 1 and 2
           (or (eq? method 'HEAD)
               (regexp-match? #rx"^1.." status-code) ;; Informational
               (regexp-match? #rx"^204" status-code) ;; No Content
               (regexp-match? #rx"^304" status-code) ;; Not Modified
               (and (eq? method 'CONNECT)
                    (regexp-match? #rx"^2.." status-code))))
         (check-headers method decodes no-content? headers)
         (define close? (or iclose? (send headers has-value? 'connection #"close")))
         (when close? (abandon))
         (define decoded-content
           (cond [(not no-content?)
                  (define raw-content (make-raw-content-input no-content? headers))
                  (make-decoded-content-input headers raw-content)]
                 [else #f]))
         (when close? (close))
         (values status-line http-version status-code headers decoded-content))))

    (define/public (read-status-line)
      (define line (b-read-bytes-line br STATUS-EOL-MODE))
      (match (regexp-match (rx STATUS-LINE) line)
        [(list _ http-version status-code reason-phrase)
         (values line http-version status-code)]
        [#f (comm-error "expected status line from server\n  got: ~e" line)]))

    (define/public (read-raw-headers)
      (define next (b-read-bytes-line br HEADER-EOL-MODE))
      (cond [(equal? next #"") null]
            [else (cons next (read-raw-headers))]))

    (define/private (check-headers method decodes no-content? headers)
      (when (send headers has-key? 'content-length)
        (send headers check-value 'content-length bytes->nat "nonnegative integer"))
      (when (send headers has-key? 'transfer-encoding)
        (send headers check-value 'transfer-encoding
              (lambda (b) (equal? b #"chunked"))
              (format "~s" #"chunked")))
      (when (send headers has-key? 'content-encoding)
        (send headers check-value 'content-encoding
              (lambda (b) (memq (bytes->symbol b) decodes))
              (format "member of ~s" decodes)))
      ;; FIXME: others?
      (void))

    (define/private (make-raw-content-input no-content? headers) ;; -> Bytes or InputPort
      (define (done) (release-input-mutex))
      ;; Reference: https://tools.ietf.org/html/rfc7230, Section 3.3.3 (Message Body Length)
      (cond
        [no-content? #""] ;; Cases 1, 2
        [(send headers has-value? 'transfer-encoding #"chunked") ;; Case 3
         ;; FIXME: Transfer-Encoding is more complicated...
         (make-chunked-input-port done)]
        [(send headers get-nat-value 'content-length) ;; Case 5 (FIXME: detect errors, Case 4)
         => (lambda (len) (make-length-limited-input len done))]
        [else ;; Case 7
         (log-http-debug "response without Transfer-Encoding or Content-Length")
         (abandon)
         (make-read-until-eof-input-port done)]))

    (define/private (make-decoded-content-input headers raw-content) ;; Bytes or InputPort
      ;; FIXME: shouldn't checking Content-Encoding against decodes be done earlier?
      (define (get-raw-content-in) ;; -> InputPort
        (if (bytes? raw-content) (open-input-bytes raw-content) raw-content))
      (cond [(and (send headers has-value? 'content-encoding #"gzip"))
             (make-gunzip-input (get-raw-content-in))]
            [(and (send headers has-value? 'content-encoding #"deflate"))
             (make-inflate-input (get-raw-content-in))]
            [else raw-content]))

    (define/private (make-read-until-eof-input-port done)
      (define (read-until-eof in out)
        (let loop ()
          (define next (read-bytes PIPE-SIZE in))
          (cond [(eof-object? next) (done)]
                [else (begin (write-bytes next out) (loop))])))
      (make-decoding-input-port in read-until-eof))

    (define/private (make-chunked-input-port done)
      (define (unchunk-through-ports br out)
        (define (read-chunk-size)
          (define line (b-read-bytes-line br CHUNKED-EOL-MODE))
          (match (regexp-match #rx"^([0-9a-fA-F]+)(?:$|;)" line) ;; ignore chunk-ext
            [(list _ size-bs) (string->number (bytes->string/latin-1 size-bs) 16)]
            [#f (comm-error "expected valid chunk length from server\n  got: ~e" line)]))
        (define (expect-crlf)
          (let ([crlf (b-read-bytes br 2)])
            (unless (equal? crlf #"\r\n")
              (comm-error "expected CRLF after chunk\n  received: ~e" crlf))))
        (define (read/discard-trailer)
          (define line (b-read-bytes-line br CHUNKED-EOL-MODE))
          (unless (equal? line #"") (read/discard-trailer)))
        (let loop ()
          (define chunk-size (read-chunk-size))
          (cond [(zero? chunk-size)
                 (read/discard-trailer)
                 (done)]
                [else
                 (define chunk-data (b-read-bytes br chunk-size))
                 (write-bytes chunk-data out)
                 (expect-crlf)
                 (loop)])))
      (make-decoding-input-port br unchunk-through-ports))

    (define/private (make-length-limited-input len done) ;; -> InputPort or Bytes
      (cond [(<= len CONTENT-LENGTH-READ-NOW)
             (begin0 (b-read-bytes br len) (done))]
            [else
             (define (copy/length-through-ports in out)
               (define buf (make-bytes PIPE-SIZE))
               (let loop ([got 0])
                 (cond [(< got len)
                        (define n (read-bytes! buf in 0 (min PIPE-SIZE (- len got))))
                        (cond [(eof-object? n)
                               (comm-error "server closed connection (before end of content)")]
                              [else
                               (write-bytes buf out 0 n)
                               (loop (+ got n))])]
                       [else (done)])))
             (make-decoding-input-port in copy/length-through-ports)]))

    (define/private (make-gunzip-input in)
      (cond [(eof-object? (peek-byte in)) #""]
            [else (make-decoding-input-port in gunzip-through-ports)]))

    (define/private (make-inflate-input in)
      (cond [(eof-object? (peek-byte in)) #""]
            [else (make-decoding-input-port in inflate)]))

    (define/private (make-decoding-input-port src decode-through-ports)
      (define-values (in out) (make-pipe PIPE-SIZE))
      (define wrapped-in (make-async-exn-input-port in))
      (define decode-t
        (thread
         (lambda ()
           (with-handlers ([exn:fail?
                            (lambda (e)
                              (async-exn-input-port-raise wrapped-in e))])
             (decode-through-ports src out)
             (close-output-port out)))))
      wrapped-in)

    ))


;; ------------------------------------------------------------

(define (url->bytes u)
  ;; FIXME: UTF-8 ???
  (string->bytes/utf-8 (url->string u)))
