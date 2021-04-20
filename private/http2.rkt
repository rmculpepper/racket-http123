#lang racket/base
(require racket/class
         racket/match
         binaryio/reader
         openssl
         "interfaces.rkt"
         "header.rkt"
         "io.rkt"
         "request.rkt"
         "response.rkt"
         "h2-frame.rkt"
         "h2-pack.rkt"
         "h2-stream.rkt")
(provide (all-defined-out))

;; References:
;; - https://tools.ietf.org/html/rfc7540

(define http2-alpn-protocol #"h2")
(define http2-client-preface #"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n")

(define init-config
  (hasheq 'header-table-size 4096
          'enable-push 1
          'max-concurrent-streams +inf.0
          'initial-window-size INIT-FLOW-WINDOW
          'max-frame-size (expt 2 14)
          'max-header-list-size +inf.0))

;; FIXME: temporary
(define (connect host port)
  (define-values (in out) (ssl-connect host port #:alpn '(#"h2")))
  (unless (equal? (ssl-get-alpn-selected in) #"h2")
    (error 'connect "failed to negotiate h2"))
  (new http2-actual-connection% (in in) (out out)))

#;
(begin (define hs '((#"user-agent" #"Racket (http123)") (#"accept-encoding" #"gzip")))
       (define req (request 'GET (string->url "https://www.google.com/") hs #f))
       (define c (connect "www.google.com" 443))
       (define r (send c open-request req)))

;; ============================================================

(define http2-actual-connection%
  (class* object% ()
    (init-field in out parent)
    (super-new)

    (define br (make-binary-reader in))

    (define config init-config) ;; Config of server
    (define my-config init-config) ;; Config of client, acked by server
    (define my-configs/awaiting-ack null) ;; (Listof Config), oldest-first

    (define/public (get-config) config)
    (define/public (get-my-config) my-config)

    ;; Connection-wide limit on flow-controlled {sends, receives}.
    ;; Flow control is only applied to DATA frames (see 6.9).
    (define out-flow-window INIT-FLOW-WINDOW)
    (define in-flow-window INIT-FLOW-WINDOW)

    ;; Target for in-flow-window is init window plus sum of stream windows.
    (define target-in-flow-window in-flow-window)

    ;; FIXME: resize on SETTINGS receive or ack?
    (define reading-dt (make-dtable (hash-ref config 'header-table-size)))
    (define sending-dt (make-dtable (hash-ref config 'header-table-size)))

    (define/public (get-sending-dt) sending-dt)

    ;; closed? : (U #f 'by-goaway 'by-error 'user-abandoned 'EOF)
    ;; This state is used by the manager and by user threads.
    ;; The reader keeps reading until EOF.
    (define closed? #f)

    (define/private (set-closed! reason)
      (unless closed?
        (set! closed? reason)
        (abandon-port out)))

    (define/public (abandon) (set-closed! 'user-abandoned))

    (define/public (open?) (not closed?))

    ;; ----------------------------------------

    (define/public (queue-frame fr)
      (log-http2-debug "--> ~a~a" (if closed? "DROPPING " "")
                       (parameterize ((error-print-width 60))
                         (format "~e" fr)))
      (unless closed? (write-frame out fr)))
    (define/public (queue-frames frs)
      (for ([fr (in-list frs)]) (queue-frame fr)))
    (define/public (flush-frames) (flush-output out))

    (define/public (connection-error errorcode [comment ""] #:debug [debug #""])
      (log-http2-debug "connection-error ~s, ~s" errorcode comment)
      (queue-frame (frame type:GOAWAY 0 0 (fp:goaway last-server-streamid errorcode debug)))
      (flush-frames)
      ;; FIXME: notify all streams
      (set-closed! 'by-error)
      (when #t
        (let ([fake-exn (exn:fail (format "connection error: ~s, ~s" errorcode comment)
                                  (current-continuation-marks))])
          ((error-display-handler) (exn-message fake-exn) fake-exn)))
      (raise 'connection-error))

    ;; ----------------------------------------

    ;; stream-table : Hash[Nat => stream%]
    ;; If a streamid has no entry in stream-table and it is less than
    ;; or equal to last-{s,c}-streamid, it is closed.
    (define stream-table (make-hasheqv))
    (define streams-changed? #f)
    (define last-server-streamid 0) ;; Nat -- last streamid used by server
    (define last-client-streamid 1) ;; Nat -- last streamid used by client

    (define/public (get-stream streamid [fail-ok? #f])
      (cond [(zero? streamid)
             (connection-error error:PROTOCOL_ERROR "get streamid = 0")]
            [(hash-ref stream-table streamid #f)
             => values]
            [(and (streamid-from-server? streamid)
                  (> streamid last-server-streamid))
             (make-stream streamid #f #f)]
            [fail-ok? #f]
            [else (connection-error error:STREAM_CLOSED "unknown stream")]))

    (define/public (remove-stream streamid)
      (set! streams-changed? #t)
      (hash-remove! stream-table streamid))

    (define/public (new-client-stream req send-req?)
      (make-stream (+ last-client-streamid 2) req send-req?))

    (define/public (make-stream streamid req send-req?)
      (define stream
        (new http2-stream% (conn this) (streamid streamid) (req req) (send-req? send-req?)))
      (set! streams-changed? #t)
      (hash-set! stream-table streamid stream)
      (cond [(streamid-from-client? streamid)
             (set! last-client-streamid streamid)]
            [(streamid-from-server? streamid)
             (set! last-server-streamid streamid)])
      stream)

    ;; ----------------------------------------
    ;; Handling frames received from server

    (define/private (handle-frame-or-other v)
      (match v
        [(? frame? fr) (handle-frame fr)]
        ['EOF (unless closed?
                ;; FIXME: update all streams like goaway?
                (set-closed! 'EOF))]
        ;; FIXME: read-exn, etc?
        ))

    (define in-continue-frames null) ;; (Listof Frame), reversed

    (define/private (handle-frame fr)
      (cond [(pair? in-continue-frames)
             (define streamid (frame-streamid (car in-continue-frames)))
             (match fr
               [(frame (== type:CONTINUATION) flags (== streamid) payload)
                (cond [(flags-has? flags flag:END_HEADERS)
                       (define frs (reverse (cons fr in-continue-frames)))
                       (set! in-continue-frames null)
                       (handle-multipart-frames frs)]
                      [else (set! in-continue-frames (cons fr in-continue-frames))])]
               [_ (connection-error error:PROTOCOL_ERROR "expected continuation")])]
            [else (handle-frame* fr)]))

    (define/private (handle-multipart-frames frs)
      (define (get-header streamid first-headerbf)
        (define rest-headerbfs
          (for/list ([fr (in-list (cdr frs))])
            (fp:continuation-headerbf (frame-payload fr))))
        (define headerb (apply bytes-append first-headerbf rest-headerbfs))
        (with-handler (lambda (e)
                        (connection-error
                         error:COMPRESSION_ERROR
                         #:streamid streamid
                         #:raise (merge-exn e "error decoding headers"
                                            (hasheq 'code 'malformed-headers
                                                    'received 'yes))))
          (decode-header headerb reading-dt)))
      (match (car frs)
        [(frame (== type:HEADERS) flags streamid
                (fp:headers padlen streamdep weight headerbf))
         (define header (get-header streamid headerbf))
         (send (get-stream streamid) handle-headers-payload
               flags (fp:headers padlen streamdep weight header))]
        [(frame (== type:PUSH_PROMISE) flags streamid
                (fp:push_promise padlen promised-streamid headerbf))
         (define header (get-header streamid headerbf))
         (send (get-stream streamid) handle-push_promise-payload
               flags (fp:push_promise padlen promised-streamid header))]))

    (define/private (handle-frame* fr)
      (match fr
        [(frame type flags streamid payload)
         (define (stream) (get-stream streamid))
         (define (check-stream-zero)
           (unless (= streamid 0) (connection-error error:PROTOCOL_ERROR "want stream = 0")))
         (define (check-stream-nonzero)
           (when (= streamid 0) (connection-error error:PROTOCOL_ERROR "want stream != 0")))
         (match type
           [(== type:DATA)
            (check-stream-nonzero)
            (send (stream) handle-data-payload flags payload)]
           [(== type:HEADERS)
            (check-stream-nonzero)
            (cond [(flags-has? flags flag:END_HEADERS)
                   (handle-multipart-frames (list fr))]
                  [else (set! in-continue-frames (list fr))])]
           [(== type:PRIORITY)
            (check-stream-nonzero)
            (send (stream) handle-priority-payload payload)]
           [(== type:RST_STREAM)
            (match-define (fp:rst_stream errorcode) payload)
            (check-stream-nonzero)
            (send (stream) handle-rst_stream errorcode)]
           [(== type:SETTINGS)
            (check-stream-zero)
            (match-define (fp:settings settings) payload)
            (cond [(flags-has? flags flag:ACK)
                   (unless (null? settings)
                     (connection-error error:FRAME_SIZE_ERROR "non-empty settings ack"))
                   (unless (pair? my-configs/awaiting-ack)
                     (connection-error error:PROTOCOL_ERROR "unexpected settings ack"))
                   (set! my-config (car my-configs/awaiting-ack))
                   (set! my-configs/awaiting-ack (cdr my-configs/awaiting-ack))]
                  [else (handle-settings settings)])]
           [(== type:PUSH_PROMISE)
            (check-stream-nonzero)
            (when (zero? (hash-ref my-config 'enable-push))
              (connection-error error:PROTOCOL_ERROR "push not enabled"))
            (cond [(flags-has? flags flag:END_HEADERS)
                   (handle-multipart-frames (list fr))]
                  [else (set! in-continue-frames (list fr))])]
           [(== type:PING)
            (check-stream-zero)
            (queue-frame (frame type:PING flag:ACK 0 payload))
            (flush-frames)]
           [(== type:GOAWAY)
            (check-stream-zero)
            (match-define (fp:goaway last-streamid errorcode debug) payload)
            ;; last-streamid is the last streamid that we initiated that the server acks
            (for ([(streamid stream) (in-hash stream-table)])
              (send stream handle-goaway last-streamid errorcode debug))
            (set-closed! 'by-goaway)]
           [(== type:WINDOW_UPDATE)
            (match-define (fp:window_update increment) payload)
            (when (zero? increment) (connection-error error:PROTOCOL_ERROR "zero increment"))
            (cond [(zero? streamid)
                   (handle-connection-window_update flags increment)]
                  [else
                   (send (stream) handle-window_update flags increment)])]
           [(== type:CONTINUATION)
            (connection-error error:PROTOCOL_ERROR "unexpected continuation")]
           [_
            ;; Ignore unknown frames, per ??.
            (void)])]))

    (define/private (handle-settings settings)
      (define new-config
        (for/fold ([h config]) ([s (in-list settings)])
          (match-define (setting key value) s)
          (case key
            [(enable-push)
             (unless (memv value '(0 1))
               (connection-error error:PROTOCOL_ERROR "bad enable_push value"))]
            [(initial-window-size)
             ;; FIXME: changes current stream windows???
             (unless (< value FLOW-WINDOW-BOUND)
               (connection-error error:FLOW_CONTROL_ERROR "window too large"))])
          (hash-set h key value)))
      (set! config new-config)
      (queue-frame (frame type:SETTINGS flag:ACK 0 (fp:settings null))))

    (define/private (handle-connection-window_update flags increment)
      (set! out-flow-window (+ out-flow-window increment))
      (unless (< out-flow-window FLOW-WINDOW-BOUND)
        (connection-error error:FLOW_CONTROL_ERROR "window too large")))

    (define/public (adjust-in-flow-window increment)
      (set! in-flow-window (+ in-flow-window)))

    (define/public (adjust-target-flow-window increment)
      ;; FIXME: sending window_update frame is delayed until... ???
      (set! target-in-flow-window (+ target-in-flow-window increment)))

    (define/private (after-handle-frame)
      (when (< in-flow-window target-in-flow-window)
        (define diff (- target-in-flow-window in-flow-window))
        (queue-frame (frame type:WINDOW_UPDATE 0 0 (fp:window_update diff)))
        (set! in-flow-window target-in-flow-window))
      (flush-frames))

    ;; ============================================================
    ;; Connection threads (2 per connection)

    ;; Manager thread
    ;; - receives frames from reader thread, handles
    ;; - receives work from streams/users, handles
    ;; - must not call any user-supplied procedures, or block on any ports (okay
    ;;   to write to out)
    ;; Reader thread
    ;; - just reads frames from input, sends to manager

    ;; FIXME: make kill-safe
    ;; FIXME: periodically prune closed streams from table

    (define/private (manager)
      (define reader-evt
        (handle-evt (thread-receive-evt)
                    (lambda (tre)
                      (define fr (thread-receive))
                      (with-handlers ([exn?
                                       (lambda (e)
                                         ((error-display-handler) (exn-message e) e))])
                        (handle-frame-or-other fr)))))
      (define (streamsloop)
        (define work-evts
          (for/list ([stream (in-hash-values stream-table)])
            (send stream get-work-evt)))
        (log-http2-debug "manager updating work evts (~s)" (length work-evts))
        (define streams-evt (apply choice-evt work-evts))
        (let loop ()
          #;(log-http2-debug "manager loop")
          (with-handlers ([(lambda (e) (eq? e 'escape-without-error)) void]
                          [(lambda (e) (eq? e 'stream-error)) void])
            (sync streams-evt
                  reader-evt
                  #;(wrap-evt (alarm-evt (+ (current-inexact-milliseconds) 10000.0))
                              (lambda (ignored) (log-http2-debug "manager is bored!")))))
          (flush-frames)
          (if (begin0 streams-changed? (set! streams-changed? #f))
              (streamsloop)
              (loop))))
      (with-handlers ([(lambda (e) (eq? e 'connection-error))
                       (lambda (e) (log-http2-debug "manager stopped due to connection error"))])
        (streamsloop)))

    (define/private (reader)
      (cond [(eof-object? (peek-byte in))
             (log-http2-debug "<-- EOF")
             (thread-send manager-thread 'EOF void)
             (void)]
            [else
             (define fr (read-frame br))
             ;; FIXME: handle reading errors...
             (log-http2-debug "<-- ~a"
                              (parameterize ((error-print-width 60))
                                (format "~e" fr)))
             (thread-send manager-thread fr void)
             (reader)]))

    (define manager-thread (thread (lambda () (manager))))
    (define reader-thread (thread (lambda () (reader))))

    ;; ========================================

    ;; called by user thread
    (define/public (open-request req)
      (define stream (new-client-stream req #t))
      ;; Stream automatically sends request header.
      (define-values (user-out resp-header-bxe user-in trailerbxe)
        (send stream get-user-communication))
      ;; User thread writes request content.
      ;; FIXME: optimize, send short data bytes on stream initialization
      (match (request-data req)
        [(? bytes? data)
         (write-bytes data user-out)
         (close-output-port user-out)]
        [#f (close-output-port user-out)]
        [(? procedure? put-data)
         ;; FIXME: on escape, abort request???
         (call-with-continuation-barrier
          (lambda ()
            (put-data (lambda (data) (write-bytes data user-out)))
            (close-output-port user-out)))])
      ;; Get response header. (Note: may receive raised-exception instead!)
      (memoize/thunk-wrap-evt
       resp-header-bxe
       (lambda (get-header-entries)
         (define header-entries (get-header-entries))
         (open-request/make-response req header-entries user-in trailerbxe))))

    (define/private (open-request/make-response req header-entries user-in trailerbxe)
      (define header
        (with-handler (lambda (e)
                        (h2-error "error processing header"
                                  #:info (hasheq 'received 'yes 'wrapped-exn e)))
          (make-header-from-entries header-entries)))
      (unless (send header value-matches? ':status #rx#"[1-5][0-9][0-9]")
        (h2-error "bad or missing status from server"
                  #:info (hasheq 'received 'yes 'code 'bad-status 'header header)))
      (define status (send header get-integer-value ':status))
      (send header remove! ':status)
      (new http2-response%
           (status-code status)
           (header header)
           (content user-in)
           (trailerbxe trailerbxe)))

    ;; ========================================

    (define/private (send-handshake)
      (write-bytes http2-client-preface out)
      (define my-new-config my-config) ;; FIXME?
      (queue-frame (frame type:SETTINGS 0 0 (fp:settings null)))
      (set! my-configs/awaiting-ack (list my-new-config))
      (flush-frames))

    (send-handshake)

    ))

(define (memoize/thunk-wrap-evt base-evt handle)
  (define result #f)
  (wrap-evt base-evt
            (lambda (base-v)
              (if result
                  result
                  (with-handlers ([(lambda (e) #t)
                                   (lambda (e)
                                     (set! result (lambda () (raise e)))
                                     result)])
                    (call-with-values
                     (lambda () (handle base-v))
                     (lambda wrap-vs
                       (set! result (lambda () (apply values wrap-vs)))
                       result)))))))
