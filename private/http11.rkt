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
      (define TRIES 2)
      (let loop ([tries TRIES])
        (when (zero? tries)
          (error* "failed to send request (too many attempts)"))
        (define ac (get-actual-connection))
        (cond [(send ac queue-request req ccontrol)
               (send ac read-next-response)]
              [else
               (send ac abandon)
               (loop (sub1 tries))])))

    #;
    (define/public (async-request req ccontrol handle)
      ...)

    ))

(define default-user-agent
  (format "Racket/~a (http123)" (version)))

(define PIPE-SIZE 4096)

(define STATUS-EOL-MODE 'return-linefeed)
(define HEADER-EOL-MODE 'return-linefeed)
(define CHUNKED-EOL-MODE 'return-linefeed)
(define CONTENT-LENGTH-READ-NOW (expt 2 20)) ;; FIXME

(define SUPPORTED-CONTENT-ENCODINGS '(#"gzip" #"deflate"))

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
      (when out (abandon-port out) (set! out #f)))

    ;; queue represents a queue of requests (and their connection controls) that
    ;; have been sent but whose responses have not been received.

    (define queue-sema (make-semaphore 1))
    (define queue null) ;; (Listof (cons Request ConnectionControl))

    (define/private (enqueue rqe)
      (call-with-semaphore queue-sema
        (lambda () (set! queue (append queue (list rqe))))))

    (define/private (dequeue)
      (call-with-semaphore queue-sema
        (lambda ()
          (and (pair? queue) (begin0 (car queue) (set! queue (cdr queue)))))))

    ;; High-level SENDING connection states:
    ;; - send-any             -- basic connected state
    ;; - send-replayable      -- because (sent, unreceived) queue is non-empty
    ;; - cannot-send          -- because of sent Close or Upgrade
    (define/public (get-send-state)
      (call-with-semaphore queue-sema (lambda () (-get-send-state))))

    (define/private (-get-send-state)
      (cond [(let ([out out]) (or (not out) (port-closed? out))) 'cannot-send]
            [(null? queue) 'send-any]
            [(for/or ([e (in-list queue)]) (control:may-end? (cdr e))) 'cannot-send]
            [else 'send-replayable]))

    ;; Misc bits of useful state information:
    ;; - http-spoken?         -- are we sure the server speaks HTTP (because of a response)?
    ;; - last-send            -- time of last send we performed (related to timeout)
    #;(define http-spoken? #f)
    #;(define last-send -inf.0)

    ;; ----------------------------------------
    ;; Output Support

    (define out-sema (make-semaphore 1))
    (define/private (release-output-mutex) (semaphore-post out-sema))
    (define/private (output-mutex-held?) (semaphore-held? out-sema))

    ;; Note: This *does not* release the mutex when the procedure returns,
    ;; unless there is an error (and then it closes the connection).
    (define/private (call/acquire-output-mutex proc)
      (call/acquire-semaphore out-sema proc (lambda () (abandon))))

    ;; ----------------------------------------
    ;; Input Support

    (define br
      (make-binary-reader in
        #:error-handler
        (make-binary-reader-error-handler
         #:error (lambda (br who fmt . args)
                   (apply s-error fmt args #:code 'read))
         #:show-data? (lambda (br who) #f))))

    (define in-sema (make-semaphore 1))
    (define/private (release-input-mutex) (semaphore-post in-sema))
    (define/private (input-mutex-held?) (semaphore-held? in-sema))

    ;; Note: This *does not* release the mutex when the procedure returns,
    ;; unless there is an error (and then it closes the connection).
    (define/private (call/acquire-input-mutex proc)
      (call/acquire-semaphore in-sema proc (lambda () (close))))

    ;; ----------------------------------------
    ;; Request and Response

    (define/public (sendrecv req ccontrol)
      ;; PRE: queue is empty! (FIXME)
      (queue-request req ccontrol)
      (read-next-response))

    ;; ----------------------------------------
    ;; Request (Output)

    ;; queue-request : Request ConnectionControl -> Boolean
    ;; Returns #t if request sent and queued, #f if cannot send in current state.
    (define/public (queue-request req ccontrol)
      (check-req-headers (request-headers req))
      (call/acquire-output-mutex
       (lambda ()
         (define can-proceed?
           (case (-get-send-state)
             [(send-any) #t]
             [(send-replayable) (request:can-replay? req)]
             [(cannot-send) #f]))
         (cond [can-proceed?
                (define rqe (make-rqentry req ccontrol))
                (-send-request req ccontrol)
                (enqueue rqe)
                (release-output-mutex)
                #t]
               [else
                (release-output-mutex)
                #f]))))

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
        (when (headerset-missing? headers #rx"^(?i:User-Agent:)")
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
      (when (eq? ccontrol 'close) (abandon)))

    (define/private (check-req-headers headers)
      (define reserved-header-rxs
        '(#rx"^(?i:Host):"
          #rx"^(?i:Accept-Encoding):" ;; In principle, this belongs to a separate layer...
          #rx"^(?i:Content-Length):"
          #rx"^(?i:Connection|Keep-Alive|Upgrade):"
          #rx"^(?i:Transfer-Encoding|TE|Trailer):"))
      (for ([header (in-list headers)])
        (for ([rx (in-list reserved-header-rxs)])
          (when (regexp-match? rx header)
            (c-error "request contains header reserved for user-agent\n  header: ~e" header
                     #:code 'reserved-request-header)))))

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

    ;; ----------------------------------------
    ;; Response (Input)

    ;; PRE: `in` has input ready (as evt), even if eof
    (define/public (read-next-response)
      (call/acquire-input-mutex
       (lambda ()
         (cond [(let ([in in])
                  (or (not in)
                      (port-closed? in)
                      (eof-object? (peek-byte in))))
                #f]
               [(dequeue)
                ;; Note: must dequeue within input-mutex so that two readers
                ;; don't read in wrong order.
                => (match-lambda
                     [(rqentry req ccontrol)
                      (read-response req ccontrol)])]
               [else
                #f]))))

    (define/private (read-response req ccontrol)
      (define method (request-method req))
      (define-values (status-line status-version status-code) (read-status-line))
      (define raw-headers (read-raw-headers))
      (define headers (new headers% (raw-headers raw-headers)))
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
      (when close? (abandon))
      (define decoded-content (if no-content? #f (get-decoded-content-input headers)))
      (when close? (close))
      (new http11-response%
           (req req) (ccontrol ccontrol)
           (status-line status-line)
           (status-version status-version)
           (status-code status-code)
           (headers headers)
           (content decoded-content)))

    (define/public (read-status-line)
      (define line (b-read-bytes-line br STATUS-EOL-MODE))
      (match (regexp-match (rx STATUS-LINE) line)
        [(list _ http-version status-code reason-phrase)
         (values line http-version status-code)]
        [#f (s-error "expected status line from server\n  got: ~e" line
                     #:code 'bad-status-line)]))

    (define/public (read-raw-headers)
      (define next (b-read-bytes-line br HEADER-EOL-MODE))
      (cond [(equal? next #"") null]
            [else (cons next (read-raw-headers))]))

    (define/private (check-headers method no-content? headers)
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
      (void))

    (define/public (get-decoded-content-input headers)
      (define raw-content (make-raw-content-input headers))
      (make-decoded-content-input headers raw-content))

    (define/private (make-raw-content-input headers) ;; -> Bytes or InputPort
      (define (done) (release-input-mutex))
      ;; Reference: https://tools.ietf.org/html/rfc7230, Section 3.3.3 (Message Body Length)
      (cond [(send headers has-value? 'transfer-encoding #"chunked") ;; Case 3
             (make-chunked-input-port done)]
            [(send headers get-nat-value 'content-length) ;; Case 5
             => (lambda (len) (make-length-limited-input len done))]
            [else ;; Case 7
             (log-http-debug "response without Transfer-Encoding or Content-Length")
             (abandon)
             (make-read-until-eof-input-port done)]))

    (define/private (make-decoded-content-input headers raw-content) ;; Bytes or InputPort
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
            [#f (s-error "expected valid chunk size from server\n  got: ~e" line
                         #:code 'bad-chunk-size)]))
        (define (expect-crlf)
          (let ([crlf (b-read-bytes br 2)])
            (unless (equal? crlf #"\r\n")
              (s-error "expected CRLF after chunk\n  received: ~e" crlf
                       #:code 'bad-chunked-transfer))))
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
                               (s-error "server closed connection (before end of content)"
                                        #:code 'short-content)]
                              [else
                               (write-bytes buf out 0 n)
                               (loop (+ got n))])]
                       [else (done)])))
             (make-decoding-input-port in copy/length-through-ports)]))

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

    (define/public (get-req) req)
    (define/public (get-ccontrol) ccontrol)
    (define/public (get-status-version) status-version)
    (define/public (get-status-code) status-code)
    (define/public (get-status-line) status-line)
    (define/public (get-headers) headers)
    (define/public (get-content) content)

    ))

;; ------------------------------------------------------------

;; FIXME: distinguish between error in decoding vs error in reading connection
;; stream; can recover from the first one.

;; make-decoding-input-port : Src (Src OutputPort -> Void) -> AsyncExnInputPort
(define (make-decoding-input-port src decode-through-ports)
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

;; make-gunzip-input : InputPort -> (U Bytes InputPort)
(define (make-gunzip-input in)
  (cond [(eof-object? (peek-byte in)) #""]
        [else (make-decoding-input-port in gunzip-through-ports)]))

;; make-inflate-input : InputPort -> (U Bytes InputPort)
(define (make-inflate-input in)
  (cond [(eof-object? (peek-byte in)) #""]
        [else (make-decoding-input-port in inflate)]))

;; ------------------------------------------------------------

(define (url->bytes u)
  ;; FIXME: UTF-8 ???
  (string->bytes/utf-8 (url->string u)))

;; ------------------------------------------------------------

(define (semaphore-held? sema)
  (not (sync/timeout 0 (semaphore-peek-evt sema))))

(define (call/acquire-semaphore sema proc handle-error)
  (semaphore-wait sema)
  (with-handlers ([(lambda (e) #t)
                   (lambda (e)
                     (when (semaphore-held? sema)
                       (handle-error)
                       (semaphore-post sema))
                     (raise e))])
    (call-with-continuation-barrier proc)))
