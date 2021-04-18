#lang racket/base
(require racket/class
         racket/match
         racket/promise
         net/url-string
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
    (define queue null) ;; (Listof SendingRecords), mutated, oldest-first

    (define/private (enqueue v)
      (with-lock
        (set! queue (append queue (list v)))
        (semaphore-post queue-count-sema)))
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
    (define state 'open)

    ;; ============================================================

    (define br
      (make-binary-reader in
        #:error-handler
        (make-binary-reader-error-handler
         #:error (lambda (br who fmt . args)
                   (apply h1-error fmt args #:info (hasheq 'code 'read)))
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

    ;; open-request : Request -> BoxEvt or #f
    ;; Returns evt if request sent and queued, #f if cannot send in current state.
    (define/public (open-request req)
      (define hls (check-req-header (request-header req)))
      (start-sending)
      (cond [(and (eq? state 'open) (not send-in-progress?))
             (define resp-bxe (make-box-evt #t))
             (set! send-in-progress? #t)
             (-send-request req hls)
             (enqueue (sending req resp-bxe))
             (set! send-in-progress? #f)
             (end-sending)
             resp-bxe]
            [else
             (end-sending)
             #f]))

    (define/private (-send-request req hls)
      (match-define (request method u _ data) req)
      (fprintf out "~a ~a HTTP/1.1\r\n" method (url->bytes u))
      (begin
        ;; RFC 7230 Section 5.4 (Host): The Host header field is required, and
        ;; it must be the same as the authority component of the target URI
        ;; (minus userinfo).
        (fprintf out "host: ~a\r\n" (url->host-bytes u))
        ;; FIXME: belongs to another layer...
        (when (header-entries-missing? hls #"user-agent")
          (fprintf out "user-agent: ~a\r\n" default-user-agent))
        (when (header-entries-missing? hls #"(accept-encoding")
          (fprintf out "accept-encoding: ~a\r\n" default-accept-encoding))
        (cond [(procedure? data)
               (fprintf out "transfer-encoding: chunked\r\n")]
              [(bytes? data)
               (format "content-length: ~a\r\n" (bytes-length data))]
              [else
               ;; If no content data, don't add Content-Length header field.
               ;; FIXME!!!
               (void)]))
      (for ([hl (in-list hls)])
        (fprintf out "~a\r\n" hl))
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

    (define/private (check-req-header hs)
      (for ([h (in-list hs)])
        (match-define (list* k v _) h)
        (when (member k reserved-header-keys/bytes)
          (h1-error "request contains header field reserved for user-agent\n  field: ~e" h
                    (hasheq 'code 'reserved-request-header-field))))
      hs)

    (define/private (url->host-bytes u)
      (send parent url->host-bytes u))

    ;; ============================================================
    ;; Reader thread

    (define reader-thread (thread (lambda () (reader))))

    (define/private (reader)
      (let loop ([in? #t])
        (sync (dequeue-evt (lambda (sr)
                             (cond [(eof-object? (peek-byte in))
                                    (abandon/close-after-timeout)]
                                   [else (reader* sr)])))
              (cond [in? (wrap-evt in
                                   (lambda (ignored)
                                     (cond [(eof-object? (peek-byte in))
                                            (abandon/close-after-timeout)]
                                           [else (loop #f)])))]
                    [else never-evt]))))
    (define/private (reader* sr)
      (define resp-set? #f)
      (match-define (sending req bxe) sr)
      ((with-handlers ([exn? (lambda (e)
                               (close-from-reader)
                               (unless resp-set?
                                 (box-evt-set! bxe (lambda () (raise e))))
                               (lambda () (void)))])
         (define-values (resp close? pump) (read-response req))
         (when close? (abandon-out-from-reader))
         (box-evt-set! bxe (lambda () resp))
         (set! resp-set? #t)
         (pump)
         (cond [close? (lambda () (close-from-reader))]
               [else (lambda () (reader))]))))
    (define/private (abandon/close-after-timeout)
      (define TIMEOUT 1)
      (abandon-out-from-reader)
      (sleep TIMEOUT)
      (close-from-reader))

    (define/private (abandon-out-from-reader)
      (with-lock (set! state 'abandoned-by-reader))
      (abandon-port out)
      (send parent on-actual-disconnect this))
    (define/private (close-from-reader)
      (with-lock (set! state 'closed-by-reader))
      (close-output-port out)
      (close-input-port in)
      (send parent on-actual-disconnect this))

    ;; ----------------------------------------
    ;; Response (Input)

    (define/private (read-response req)
      (define method (request-method req))
      (define-values (status-line status-version status-code) (read-status-line))
      (define raw-header (read-raw-header))
      (define header (make-header-from-lines raw-header))
      (define no-content? ;; RFC 7230 Section 3.3.3, cases 1 and 2
        (or (eq? method 'HEAD)
            (regexp-match? #rx"^1.." status-code) ;; Informational
            (regexp-match? #rx"^204" status-code) ;; No Content
            (regexp-match? #rx"^304" status-code) ;; Not Modified
            (and (eq? method 'CONNECT)
                 (regexp-match? #rx"^2.." status-code))))
      (check-header method no-content? header)
      (define close?
        (or ;; FIXME: if we requested Connection: close
            (send header has-value? 'connection #"close")))
      (define (make-resp content trailersbxe)
        (new http11-response%
             (status-line status-line)
             (status-code (string->number (bytes->string/latin-1 status-code)))
             (header header)
             (content content)
             (trailerbxe trailersbxe)))
      (define (return content pump trailerbxe)
        (values (make-resp content trailerbxe) close? pump))
      (cond
        [no-content?
         (values (make-resp #f #f) close? void)]
        [else
         (define-values (content pump trailerbxe) (make-content-pump header))
         (values (make-resp content trailerbxe) close? pump)]))

    (define/public (read-status-line)
      (define line (b-read-bytes-line br STATUS-EOL-MODE))
      (match (regexp-match (rx STATUS-LINE) line)
        [(list _ http-version status-code reason-phrase)
         (values line http-version status-code)]
        [#f (h1-error "expected status line from server\n  got: ~e" line
                      #:info (hasheq 'code 'bad-status-line))]))

    (define/public (read-raw-header)
      (define next (b-read-bytes-line br HEADER-EOL-MODE))
      (cond [(equal? next #"") null]
            [else (cons next (read-raw-header))]))

    (define/private (check-header method no-content? header)
      (when (send header has-key? 'content-length)
        (send header check-value 'content-length bytes->nat "nonnegative integer"))
      (when (send header has-key? 'transfer-encoding)
        (send header check-value 'transfer-encoding
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
        [(send header has-value? 'transfer-encoding #"chunked") ;; Case 3
         (make-pump/chunked br)]
        [(send header get-integer-value 'content-length) ;; Case 5
         => (lambda (len)
              (cond [(< len CONTENT-LENGTH-READ-NOW)
                     (define content (b-read-bytes br len))
                     (values content void #f)]
                    [else (make-pump/content-length br len)]))]
        [else ;; Case 7
         (log-http-debug "response without Transfer-Encoding or Content-Length")
         (abandon-out-from-reader)
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
    (close-output-port out-to-user))
  (make-pump forward/content-length))

(define (make-pump/until-eof in)
  (define (forward/until-eof out-to-user)
    (let loop ()
      (define next (read-bytes PIPE-SIZE in))
      (cond [(eof-object? next)
             (close-input-port in)
             (close-output-port out-to-user)]
            [else (begin (write-bytes next out-to-user) (loop))])))
  (make-pump forward/until-eof))

(define (make-pump/chunked br)
  (define trailerbxe (make-box-evt #t))
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
               (define p (delay (make-header-from-lines trailers)))
               (box-evt-set! trailerbxe (lambda () (force p))))
             (close-output-port out-to-user)]
            [else
             (define chunk-data (b-read-bytes br chunk-size))
             (write-bytes chunk-data out-to-user)
             (expect-crlf)
             (loop)])))
  (make-pump forward/chunked trailerbxe))

;; ------------------------------------------------------------
