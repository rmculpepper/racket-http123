;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/class
         racket/match
         racket/promise
         binaryio/reader
         "interfaces.rkt"
         "header.rkt"
         "regexp.rkt"
         "io.rkt"
         "util.rkt"
         "request.rkt"
         "response.rkt")
(provide (all-defined-out))

;; References:
;; - HTTP/1.1: https://tools.ietf.org/html/rfc7230

(define STATUS-EOL-MODE 'return-linefeed)
(define HEADER-EOL-MODE 'return-linefeed)

(define-rx STATUS-CODE #px#"[0-9]{3}") ;; FIXME: or [1-5][0-9]{2} ??
(define-rx STATUS-LINE
  (rx ^ (record "HTTP/[0-9.]+") " " (record STATUS-CODE) " " (record ".*") $))

;; A Sending is (sending Request BoxEvt[Response])
(struct sending (req resp-bxe))

(define TIMEOUT-MS 10e3) ;; FIXME: config? ok default?

;; Represents an actual connection, without reconnect ability.
;; Thread-safe:
;; - Sends are done in the user thread making the request, with a lock
;;   for mutual exclusion.
;; - Reads are done in a separate reader thread.
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
    (define queue null) ;; (Listof SendingRecords) or #f, mutated, oldest-first; #f = shut down

    (define/private (enqueue v)
      (with-lock
        (cond [queue
               (set! queue (append queue (list v)))
               (semaphore-post queue-count-sema)
               #t]
              [else #f])))
    (define/private (dequeue-evt k)
      (wrap-evt queue-count-sema
                (lambda (ignored)
                  (k (with-lock
                       (begin0 (car queue)
                         (set! queue (cdr queue))))))))

    ;; A State is one of
    ;; - 'open
    ;; - 'abandoned-by-reader
    ;; - 'closed-by-reader
    ;; - 'abandoned-by-user
    (define state 'open)

    ;; {open?,closed?} : Boolean
    ;; Really, open vs closed for sending. If closed, receives may still be in progress.
    (define/public (open?) (eq? state 'open))
    (define/public (closed?) (not (closed?)))

    ;; ============================================================

    (define br
      (make-binary-reader in
        #:error-handler
        (make-binary-reader-error-handler
         #:error (lambda (br who fmt . args)
                   (apply h1-error fmt args #:info (hasheq 'code 'read)))
         #:show-data? (lambda (br who) #f))))

    (define/public (abandon)
      (abandon-out 'abandoned-by-user))

    (define/private (abandon-out new-state)
      (define old-state (with-lock (begin0 state (set! state new-state))))
      (abandon-port out)
      (when (eq? old-state 'open)
        (send parent on-actual-disconnect this)))

    ;; ============================================================
    ;; Sending Requests

    ;; Sending is done by the user thread making the request.

    (define sending-lock (make-semaphore 1))
    (define send-in-progress? #f)

    (define/private (start-sending)
      (semaphore-post sending-lock))

    (define/private (end-sending)
      (semaphore-post sending-lock))

    ;; open-request : Request -> #f or (BoxEvt (-> Response))
    ;; Returns evt if request sent and queued, #f if cannot send in current state.
    (define/public (open-request req)
      (start-sending)
      (cond [(and (eq? state 'open) (not send-in-progress?))
             (define resp-bxe (make-box-evt))
             (set! send-in-progress? #t)
             (-send-request req)
             (or (enqueue (sending req resp-bxe))
                 (connection-closed-error req 'unknown))
             (set! send-in-progress? #f)
             (end-sending)
             (log-http1-debug "request added to queue")
             resp-bxe]
            [else
             (end-sending)
             (log-http1-debug "not sending request, state = ~s~a"
                              state (if send-in-progress? " (send in progress failed)" ""))
             #f]))

    (define/private (-send-request req)
      (match-define (request method u hfields data) req)
      (log-http1-debug "start send request")
      (fprintf out "~a ~a HTTP/1.1\r\n" method (url->bytes u))
      (begin
        ;; RFC 7230 Section 5.4 (Host): The Host header field is required, and
        ;; it must be the same as the authority component of the target URI
        ;; (minus userinfo).
        (fprintf out "host: ~a\r\n" (url->host-string u))
        (cond [(procedure? data)
               (fprintf out "transfer-encoding: chunked\r\n")]
              [(bytes? data)
               (fprintf out "content-length: ~a\r\n" (bytes-length data))]
              [else
               ;; If no content data, don't add Content-Length header field.
               ;; FIXME!!!
               (void)]))
      (for ([hfield (in-list hfields)])
        (fprintf out "~a\r\n" (header-field->line hfield)))
      (fprintf out "\r\n")
      (cond [(procedure? data)
             (let ([chunk-out (chunking-output-port out)])
               ;; FIXME: If data throws exception, then close port and bail out.
               (dynamic-wind
                 void
                 (lambda ()
                   (call-with-continuation-barrier
                    (lambda () (data chunk-out))))
                 (lambda () (close-output-port chunk-out))))
             (fprintf out "0\r\n\r\n")]
            [(bytes? data)
             (write-bytes data out)]
            [else (void)])
      (flush-output out)
      (log-http1-debug "end send request"))

    ;; ============================================================
    ;; Reader thread

    (define reader-thread
      (thread
       (lambda ()
         (parameterize ((uncaught-exception-handler
                         (let ([h (uncaught-exception-handler)])
                           (lambda (e)
                             (log-http1-error "unhandled exception in reader thread: ~e" e)
                             (h e)))))
           (reader)))))

    (define/private (reader)
      ;; Waiting for either SR from queue or EOF from server (if in?=#t).
      (let loop ([in? #t])
        (define eof-evt
          (handle-evt (if in? in never-evt)
                      (lambda (ignored)
                        (if (eof-object? (peek-byte in)) (reader/eof #f) (loop #f)))))
        (define timeout-evt
          (handle-evt (alarm-evt (+ (current-inexact-milliseconds) TIMEOUT-MS))
                      (lambda (ignored) (reader/timeout))))
        (sync (dequeue-evt (lambda (sr) (reader/sr sr)))
              eof-evt
              timeout-evt)))

    ;; Got SR from queue; try to read response (or EOF) from server.
    (define/private (reader/sr sr)
      (cond [(eof-object? (peek-byte in)) (reader/eof sr)]
            [else (reader/sr* sr)]))
    (define/private (reader/sr* sr)
      (log-http1-debug "reading response")
      (match-define (sending req bxe) sr)
      ((with-handlers ([exn? (lambda (e)
                               (log-http1-debug "error reading response from server: ~e" e)
                               (close-from-reader 'error #t #f)
                               (define e*
                                 (build-exn "error reading response from server"
                                            (hasheq 'request req
                                                    'version 'http/1.1
                                                    'received 'yes
                                                    'code 'error-reading-response
                                                    'wrapped-exn e)))
                               (box-evt-set! bxe (lambda () (raise e*)))
                               (lambda ()
                                 (log-http1-debug "ending reader loop due to error")
                                 (void)))])
         (define-values (resp close? pump) (read-response req))
         (when close?
           (log-http1-debug "got Connection:close from server")
           (abandon-out 'abandoned-by-reader))
         (box-evt-set! bxe (lambda () resp))
         (pump)
         (cond [close? (lambda () (reader/close))]
               [else (lambda () (reader))]))))

    (define/private (reader/timeout)
      (log-http1-debug "timeout")
      (close-from-reader 'timeout #t #f)
      (log-http1-debug "ending reader loop due to timeout"))

    ;; Got EOF from server at the beginning of a response.
    (define/private (reader/eof sr)
      (log-http1-debug "got EOF from server")
      (close-from-reader 'eof #t sr)
      (log-http1-debug "ending reader loop due to EOF from server"))

    ;; Got "Connection: close" from server in previous response.
    (define/private (reader/close)
      (fail-queue 'close 'no)
      (close-from-reader 'close #f)
      (log-http1-debug "ending reader loop due to Connection:close from server"))

    (define/private (close-from-reader why fail-queue? [extra-sr #f])
      (define old-state (with-lock (begin0 state (set! state 'closed-by-reader))))
      (close-output-port out)
      (close-input-port in)
      (when fail-queue? (fail-queue why 'unknown extra-sr))
      (when (eq? old-state 'open)
        (send parent on-actual-disconnect this)))

    (define/private (fail-queue why received [extra-sr #f])
      (let* ([queue (or (with-lock (begin0 queue (set! queue #f))) null)]
             [queue (if extra-sr (cons extra-sr queue) queue)])
        (log-http1-debug "failing ~s queued requests with status: ~e" (length queue) received)
        (for ([sr (in-list queue)])
          (match-define (sending req resp-bxe) sr)
          (box-evt-set! resp-bxe (lambda () (connection-closed-error why req received))))))

    (define/private (connection-closed-error why req received)
      (h1-error (case why
                  [(timeout) "connection closed by user agent (timeout)"]
                  [(error) "connection closed due to read error"]
                  [(close) "connection closed by server (Connection: close)"]
                  [(eof) "connection closed by server (EOF)"])
                #:info (hasheq 'code (case why
                                       [(timeout) 'ua-timeout]
                                       [(error) 'read-error]
                                       [(close) 'server-closed]
                                       [(eof) 'server-EOF])
                               'request req
                               'received received)))

    ;; ----------------------------------------
    ;; Response (Input)

    (define/private (read-response req)
      (define-values (status-line status-version status-code) (read-status-line))
      (define raw-header (read-raw-header))
      (define header (make-header-from-lines raw-header))
      (cond [(regexp-match? #rx"^1.." status-code) ;; Informational
             (log-http1-debug "discarding Informational response")
             (read-response req)]
            [else (read-response* req status-line status-code header)]))

    (define/private (read-response* req status-line status-code header)
      (define method (request-method req))
      (log-http1-debug "got header")
      (define no-content? ;; RFC 7230 Section 3.3.3, cases 1 and 2
        (or (eq? method 'HEAD)
            (regexp-match? #rx"^1.." status-code) ;; Informational, but see above
            (regexp-match? #rx"^204" status-code) ;; No Content
            (regexp-match? #rx"^304" status-code) ;; Not Modified
            (and (eq? method 'CONNECT)
                 (regexp-match? #rx"^2.." status-code))))
      (check-header method no-content? header)
      (define close?
        (or ;; FIXME: if we requested Connection: close
            ;; FIXME: more robust value comparison
            (send header has-value? #"connection" #"close")))
      (define (make-resp content trailersbxe)
        (new http11-response%
             (request req)
             (status-line status-line)
             (status-code (string->number (bytes->string/latin-1 status-code)))
             (header header)
             (content content)
             (trailerbxe trailersbxe)))
      (define (return content pump trailerbxe)
        (values (make-resp content trailerbxe) close? pump))
      (cond
        [no-content?
         (log-http1-debug "no content")
         (values (make-resp #f #f) close? void)]
        [else
         (define-values (content pump trailerbxe) (make-content-pump header))
         (values (make-resp content trailerbxe) close? pump)]))

    (define/private (read-status-line)
      (define line (b-read-bytes-line br STATUS-EOL-MODE))
      (log-http1-debug "got status line: ~e" line)
      (match (regexp-match (rx STATUS-LINE) line)
        [(list _ http-version status-code reason-phrase)
         (values line http-version status-code)]
        [#f (h1-error "expected status line from server\n  got: ~e" line
                      #:info (hasheq 'code 'bad-status-line))]))

    (define/private (read-raw-header)
      (define next (b-read-bytes-line br HEADER-EOL-MODE))
      (cond [(equal? next #"") null]
            [else (cons next (read-raw-header))]))

    (define/private (check-header method no-content? header)
      (when (send header has-key? #"content-length")
        (send header check-value #"content-length" bytes->nat "nonnegative integer"))
      (when (send header has-key? #"transfer-encoding")
        (send header check-value #"transfer-encoding"
              (lambda (b) (equal? b #"chunked"))
              (format "~s" #"chunked")))
      ;; FIXME: others?
      (void))

    ;; make-content-pump : Header -> (values Content (-> Void))
    ;; The pump procedure can raise an exception (for example, to signal the
    ;; server has closed the connection), but if it does, it must also propagate
    ;; it to the content result (usually a wrapped input port).
    (define/private (make-content-pump header)
      (cond
        ;; Reference: https://tools.ietf.org/html/rfc7230, Section 3.3.3 (Message Body Length)
        [(send header has-value? #"transfer-encoding" #"chunked") ;; Case 3
         (log-http1-debug "reading content (Transfer-Encoding: chunked)")
         (make-pump/chunked br)]
        [(send header get-integer-value #"content-length") ;; Case 5
         => (lambda (len)
              (log-http1-debug "reading content (Content-Length: ~a)" len)
              (cond [(< len CONTENT-LENGTH-READ-NOW)
                     (define content (b-read-bytes br len))
                     (values content void #f)]
                    [else (make-pump/content-length br len)]))]
        [else ;; Case 7
         (log-http1-debug "reading content (until EOF)")
         (abandon-out 'abandoned-by-reader)
         (make-pump/until-eof in)]))
    ))

;; ------------------------------------------------------------

(define PIPE-SIZE 4096)
(define CHUNKED-EOL-MODE 'return-linefeed)
(define CONTENT-LENGTH-READ-NOW (expt 2 20)) ;; FIXME

(define (make-pump proc [trailersbxe #f])
  (define-values (wrapped-user-in out-to-user raise-user-exn) (make-wrapped-pipe))
  (values wrapped-user-in
          (lambda ()
            (with-handlers ([exn? (lambda (e) (raise-user-exn e) (raise e))])
              (proc out-to-user)))
          trailersbxe))

(define (make-pump/content-length br len)
  (define (forward/content-length out-to-user)
    (write-bytes (b-read-bytes br len) out-to-user)
    (log-http1-debug "finished reading message body (Content-Length)")
    (close-output-port out-to-user))
  (make-pump forward/content-length))

(define (make-pump/until-eof in)
  (define (forward/until-eof out-to-user)
    (let loop ()
      (define next (read-bytes PIPE-SIZE in))
      (cond [(eof-object? next)
             (log-http1-debug "finished reading message body (EOF)")
             (close-input-port in)
             (close-output-port out-to-user)]
            [else (begin (write-bytes next out-to-user) (loop))])))
  (make-pump forward/until-eof))

(define (make-pump/chunked br)
  (define trailerbxe (make-box-evt))
  (define (forward/chunked out-to-user)
    (define (read-chunk-size)
      (define line (b-read-bytes-line br CHUNKED-EOL-MODE))
      (match (regexp-match #rx"^([0-9a-fA-F]+)(?:$|;)" line) ;; ignore chunk-ext
        [(list _ size-bs) (string->number (bytes->string/latin-1 size-bs) 16)]
        [#f (h1-error "expected valid chunk size from server\n  got: ~e" line
                      #:info (hasheq 'received 'yes 'code 'bad-chunked-transfer))]))
    (define (expect-crlf)
      (let ([crlf (b-read-bytes br 2)])
        (unless (equal? crlf #"\r\n")
          (h1-error "expected CRLF after chunk\n  received: ~e" crlf
                    #:info (hasheq 'received 'yes 'code 'bad 'chunked-transfer)))))
    (define (read-trailer)
      (define line (b-read-bytes-line br CHUNKED-EOL-MODE))
      (cond [(equal? line #"") null]
            [else (cons line (read-trailer))]))
    (let loop ()
      (define chunk-size (read-chunk-size))
      (cond [(zero? chunk-size)
             (let ([trailers (read-trailer)])
               (define p (delay/sync (make-header-from-lines trailers)))
               (box-evt-set! trailerbxe (lambda () (force p))))
             (log-http1-debug "finished reading message body (Transfer-Encoding: chunked)")
             (close-output-port out-to-user)]
            [else
             (define chunk-data (b-read-bytes br chunk-size))
             (write-bytes chunk-data out-to-user)
             (expect-crlf)
             (loop)])))
  (make-pump forward/chunked trailerbxe))

;; ------------------------------------------------------------

;; chunking-output-port : OutputPort Boolean [PosInt] -> OutputPort
;; Note: does not send the final "0\r\n".
(define (chunking-output-port out [max-flush-chunk-length 4096])
  (define-values (buf-in buf-out) (make-pipe))
  (define chunk #f)
  (define chunk-sent 0)
  (define flush-lock (make-semaphore 1))
  (define (write-out buf start end non-block? eb?)
    (cond [(< start end)
           (cond [non-block?
                  ;; This port can't honor the "must not buffer" part of the
                  ;; output-port interface; it doesn't make much sense in this
                  ;; case. Instead, write to buffer and then (maybe) try to flush.
                  (write-bytes buf buf-out start end)
                  (when #f ;; FIXME: does flushing here even make sense?
                    (when (semaphore-try-wait? flush-lock)
                      (try-flush-without-block)
                      (semaphore-post flush-lock)))
                  (- end start)]
                 [else buf-out])]
          [else ;; flush => non-block? = #f (allowed to block)
           (begin (flush eb?) 0)]))
  (define (try-flush-without-block) ;; returns #t if flush completed
    ;; PRE: holding flush-lock, breaks disabled
    (cond [chunk
           (define r (write-bytes-avail* chunk out chunk-sent))
           (cond [(or (eq? r #f) (zero? r))
                  #f]
                 [(< (+ chunk-sent r) (bytes-length chunk))
                  (set! chunk-sent (+ chunk-sent r))
                  (try-flush-without-block)]
                 [else
                  (set! chunk #f)
                  (try-flush-without-block)])]
          [(positive? (pipe-content-length buf-in))
           (define buffered (pipe-content-length buf-in))
           (define len (min buffered (or max-flush-chunk-length buffered)))
           (let ([chunk-data (read-bytes len buf-in)])
             (define enc-len (string->bytes/utf-8 (format "~X" len)))
             (set! chunk (bytes-append enc-len #"\r\n" chunk-data #"\r\n"))
             (set! chunk-sent 0))
           (try-flush-without-block)]
          [else #t]))
  (define (flush/can-block eb?) ;; PRE: holding flush-lock
    (cond [(try-flush-without-block) (void)]
          [else (begin (if eb? (sync/enable-break out) (sync out)) (flush/can-block))]))
  (define (flush eb?)
    (parameterize-break #f
      (if eb? (semaphore-wait/enable-break flush-lock) (semaphore-wait flush-lock))
      (flush/can-block eb?)
      (semaphore-post flush-lock)))
  (define (close)
    (flush #t))
  (make-output-port* #:name (object-name out)
                     #:evt always-evt
                     #:write-out write-out
                     #:close close))
