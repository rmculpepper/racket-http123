#lang racket/base
(require racket/class
         racket/match
         racket/list
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
         "request.rkt"
         "decode.rkt"
         file/gunzip)
(provide (all-defined-out))

;; References:
;; - HTTP/1.1: https://tools.ietf.org/html/rfc7230

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
      (cond [(and conn (send conn live?))
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

    (define/public (sync-request req ccontrol)
      (sync (async-request req ccontrol)))

    (define/public (async-request req ccontrol)
      (define TRIES 2)
      (let loop ([tries TRIES])
        (when (zero? tries)
          (error* "failed to send request (too many attempts)"))
        (define ac (get-actual-connection))
        (cond [(send ac open-request req ccontrol) => values]
              [else (begin (send ac abandon) (loop (sub1 tries)))])))

    ))

(define STATUS-EOL-MODE 'return-linefeed)
(define HEADER-EOL-MODE 'return-linefeed)

(define SUPPORTED-CONTENT-ENCODINGS '(#"gzip" #"deflate"))

(define-rx STATUS-CODE #px#"[0-9]{3}")
(define-rx STATUS-LINE
  (rx ^ (record "HTTP/[0-9.]+") " " (record STATUS-CODE) " " (record ".*") $))

;; A Sending is (sending Request CControl BoxEvt[Response])
(struct sending (req ccontrol resp-bxe))

;; Represents an actual connection, without reconnect ability.
;; Not thread-safe: expects at most one sending thread, one reading thread.
(define http11-actual-connection%
  (class* object% (#; http-connection<%>)
    (init-field parent in out [try-upgrade? #t])

    ;; FIXME: implement try-upgrade?

    (super-new)

    ;; ============================================================
    ;; Shared state

    (define lock (make-semaphore 1))
    (define-syntax-rule (with-lock e ...) ;; doesn't unlock on escape
      (begin (semaphore-wait lock) (begin0 (let () e ...) (semaphore-post lock))))

    (define queue-count-sema (make-semaphore 0))
    (define queue null) ;; (Listof SendingRecords), mutated, oldest-first

    (define/private (enqueue v)
      (with-lock
        (set! queue (append queue (list v)))
        (semaphore-post queue-count-sema)))
    (define/private (dequeue)
      (semaphore-wait queue-count-sema)
      (with-lock (begin0 (and (pair? queue) (begin0 (car queue) (set! queue (cdr queue)))))))

    ;; A State is one of
    ;; - 'open
    ;; - 'abandoned-by-reader
    ;; - 'closed-by-reader
    (define state 'open)

    ;; ============================================================

    (define br
      (make-binary-reader in
        #:error-handler
        (make-binary-reader-error-handler
         #:error (lambda (br who fmt . args)
                   (apply h-error fmt args #:code 'read #:party 'server))
         #:show-data? (lambda (br who) #f))))

    (define/public (live?)
      (and (let ([in in]) (and in (not (port-closed? in))))
           (let ([out out]) (and out (not (port-closed? out))))))

    (define/private (abandon-in) (abandon-port in))
    (define/private (abandon-out) (abandon-port out))
    (define/public (abandon) (abandon-out))

    ;; Misc bits of useful state information:
    ;; - http-spoken?         -- are we sure the server speaks HTTP (because of a response)?
    ;; - last-send            -- time of last send we performed (related to timeout)
    #;(define http-spoken? #f)
    #;(define last-send -inf.0)

    ;; ============================================================
    ;; Sending Requests

    ;; Sending is done by the user thread making the request.

    (define sending-lock (make-semaphore 1))
    (define send-in-progress? #f)

    (define/private (start-sending)
      (semaphore-post sending-lock))

    (define/private (end-sending)
      (semaphore-post sending-lock))

    ;; open-request : Request ConnectionControl -> BoxEvt or #f
    ;; Returns evt if request sent and queued, #f if cannot send in current state.
    (define/public (open-request req ccontrol)
      (check-req-headers (request-headers req))
      (start-sending)
      (cond [(and (eq? state 'open) (not send-in-progress?))
             (define resp-bxe (make-box-evt #t))
             (set! send-in-progress? #t)
             (-send-request req ccontrol)
             (enqueue (sending req ccontrol resp-bxe))
             (set! send-in-progress? #f)
             (end-sending)
             resp-bxe]
            [else
             (end-sending)
             #f]))

    (define/private (-send-request req ccontrol)
      (match-define (request method u headers data) req)
      (fprintf out "~a ~a HTTP/1.1\r\n" method (url->bytes u))
      (begin
        ;; RFC 7230 Section 5.4 (Host): The Host header is required,
        ;; and it must be the same as the authority component of the
        ;; target URI (minus userinfo).
        (fprintf out "Host: ~a\r\n" (url->host-bytes u))
        (fprintf out "Accept-Encoding: ~a\r\n"
                 (bytes-join SUPPORTED-CONTENT-ENCODINGS #","))
        (when (headerlines-missing? headers #rx"^(?i:User-Agent:)")
          (fprintf out "User-Agent: ~a\r\n" default-user-agent))
        (cond [(procedure? data)
               (fprintf out "Transfer-Encoding: chunked\r\n")]
              [(bytes? data)
               (format "Content-Length: ~a\r\n" (bytes-length data))]
              [else
               ;; If no content data, don't add Content-Length header.
               ;; FIXME!!!
               (void)])
        (send-connection-control ccontrol))
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
      (flush-output out)
      (when (eq? ccontrol 'close) (abandon-out)))

    (define/private (check-req-headers headers)
      (for ([header (in-list headers)])
        (for ([rx (in-list reserved-headerline-rxs)])
          (when (regexp-match? rx header)
            (h-error "request contains header reserved for user-agent\n  header: ~e" header
                     #:party 'user #:code 'reserved-request-header)))))

    (define/private (url->host-bytes u)
      (send parent url->host-bytes u))

    (define/private (send-connection-control ccontrol)
      (match ccontrol
        ['close
         (fprintf out "Connection: close\r\n")]
        [(or 'keep-alive #f)
         (void)]
        [(connection:upgrade protocols)
         (fprintf out "Connection: upgrade\r\n")
         (fprintf out "Upgrade: ~a\r\n"
                  (bytes-join protocols #", "))]))

    ;; ============================================================
    ;; Reader thread

    (define reader-thread (thread (lambda () (reader))))

    (define/private (reader)
      (define sr (dequeue))
      (define resp-set? #f)
      (match-define (sending req ccontrol bxe) sr)
      ((with-handlers ([exn? (lambda (e)
                               (close-from-reader)
                               (unless resp-set?
                                 (box-evt-set! bxe (lambda () (raise e))))
                               (lambda () (void)))])
         (define-values (resp close? pump) (read-response req ccontrol))
         (when close? (abandon-out-from-reader))
         (box-evt-set! bxe (lambda () resp))
         (set! resp-set? #t)
         (pump)
         (cond [close? (lambda () (close-from-reader))]
               [else (lambda () (reader))]))))

    (define/private (abandon-out-from-reader)
      (with-lock (set! state 'abandoned-by-reader))
      (abandon-port out))
    (define/private (close-from-reader)
      (with-lock (set! state 'closed-by-reader))
      (close-output-port out)
      (close-input-port in))

    ;; ----------------------------------------
    ;; Response (Input)

    (define/private (read-response req ccontrol)
      (define method (request-method req))
      (define-values (status-line status-version status-code) (read-status-line))
      (define raw-headers (read-raw-headers))
      (define headers
        (parameterize ((h-error-info (hasheq 'party 'server)))
          (make-headers-from-lines raw-headers)))
      (define no-content? ;; RFC 7230 Section 3.3.3, cases 1 and 2
        (or (eq? method 'HEAD)
            (regexp-match? #rx"^1.." status-code) ;; Informational
            (regexp-match? #rx"^204" status-code) ;; No Content
            (regexp-match? #rx"^304" status-code) ;; Not Modified
            (and (eq? method 'CONNECT)
                 (regexp-match? #rx"^2.." status-code))))
      (check-headers method no-content? headers)
      (define close?
        (or (eq? ccontrol 'close)
            (send headers has-value? 'connection #"close")))
      (define (make-resp content)
        (new http11-response%
             (req req) (ccontrol ccontrol)
             (status-line status-line)
             (status-version status-version)
             (status-code status-code)
             (headers headers)
             (content content)))
      (define (return content pump)
        (values (make-resp content) close? pump))
      (cond
        [no-content?
         (values (make-resp #f) close? void)]
        [else
         (define-values (content pump) (make-content-pump headers))
         (values (make-resp content) close? pump)]))

    (define/public (read-status-line)
      (define line (b-read-bytes-line br STATUS-EOL-MODE))
      (match (regexp-match (rx STATUS-LINE) line)
        [(list _ http-version status-code reason-phrase)
         (values line http-version status-code)]
        [#f (h-error "expected status line from server\n  got: ~e" line
                     #:party 'server #:code 'bad-status-line)]))

    (define/public (read-raw-headers)
      (define next (b-read-bytes-line br HEADER-EOL-MODE))
      (cond [(equal? next #"") null]
            [else (cons next (read-raw-headers))]))

    (define/private (check-headers method no-content? headers)
      (parameterize ((h-error-info (hasheq 'party 'server)))
        (when (send headers has-key? 'content-length)
          (send headers check-value 'content-length bytes->nat "nonnegative integer"))
        (when (send headers has-key? 'transfer-encoding)
          (send headers check-value 'transfer-encoding
                (lambda (b) (equal? b #"chunked"))
                (format "~s" #"chunked")))
        (when (send headers has-key? 'content-encoding)
          (send headers check-value 'content-encoding
                (lambda (b) (member b SUPPORTED-CONTENT-ENCODINGS))  ;; FIXME: case-insensitive?
                (format "member of ~a" SUPPORTED-CONTENT-ENCODINGS)))
        ;; FIXME: others?
        (void)))

    ;; make-content-pump : Headers -> (values Content (-> Void))
    ;; The pump procedure can raise an exception (for example, to signal the
    ;; server has closed the connection), but if it does, it must also propagate
    ;; it to the content result (usually a wrapped input port).
    (define/private (make-content-pump headers)
      (define decode-mode (get-decode-mode headers))
      (cond
        ;; Reference: https://tools.ietf.org/html/rfc7230, Section 3.3.3 (Message Body Length)
        [(send headers has-value? 'transfer-encoding #"chunked") ;; Case 3
         (make-pump/chunked br decode-mode)]
        [(send headers get-integer-value 'content-length) ;; Case 5
         => (lambda (len)
              (cond [(and (< len CONTENT-LENGTH-READ-NOW) (eq? decode-mode #f))
                     (define content (b-read-bytes br len))
                     (values content void)]
                    [else (make-pump/content-length br len decode-mode)]))]
        [else ;; Case 7
         (log-http-debug "response without Transfer-Encoding or Content-Length")
         (abandon-out-from-reader)
         (make-pump/until-eof in decode-mode)]))

    ))

;; ------------------------------------------------------------

(define http11-response%
  (class* object% ()
    (init-field req ccontrol
                status-version  ;; Bytes
                status-code     ;; Nat
                status-line     ;; Bytes
                headers         ;; headers%
                content)        ;; Bytes or InputPort
    (super-new)

    (define/public (get-request) req)
    (define/public (get-ccontrol) ccontrol)
    (define/public (get-status-version) status-version)
    (define/public (get-status-code) status-code)
    (define/public (get-status-line) status-line)
    (define/public (get-headers) headers)
    (define/public (get-content) content)

    ))

;; ------------------------------------------------------------

;; FIXME: remove fragment, etc
(define (url->bytes u)
  ;; FIXME: UTF-8 ???
  (string->bytes/utf-8 (url->string u)))
