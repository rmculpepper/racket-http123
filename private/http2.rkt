#lang racket/base
(require racket/class
         racket/promise
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

(define INIT-TARGET-IN-FLOW-WINDOW (expt 2 20)) ;; FIXME: config?

(define init-config
  (hasheq 'header-table-size 4096
          'enable-push 1
          'max-concurrent-streams +inf.0
          'initial-window-size INIT-FLOW-WINDOW
          'max-frame-size (expt 2 14)
          'max-header-list-size +inf.0))

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

    (define/public (get-config-value key)
      (hash-ref config key))

    ;; Connection-wide limit on flow-controlled {sends, receives}.
    ;; Flow control is only applied to DATA frames (see 6.9).
    (define out-flow-window INIT-FLOW-WINDOW)
    (define in-flow-window INIT-FLOW-WINDOW)

    ;; FIXME: resize on SETTINGS receive or ack?
    (define reading-dt (make-dtable (hash-ref config 'header-table-size)))
    (define sending-dt (make-dtable (hash-ref config 'header-table-size)))

    (define/public (get-sending-dt) sending-dt)

    ;; is-closed? : (U #f 'by-goaway 'by-error 'user-abandoned 'EOF)
    ;; Value is #f if allowed to start new streams; symbol if not allowed.
    (define is-closed? #f)
    (define/public (open?) (not is-closed?))
    (define/public (closed?) (not (open?)))

    (define/private (set-closed! reason)
      (unless is-closed? (set! is-closed? reason)))

    ;; ----------------------------------------

    (define/public (queue-frame fr)
      (log-http2-debug "--> ~a"
                       (parameterize ((error-print-width 60))
                         (format "~e" fr)))
      (adjust-out-flow-window (- (frame-fc-length fr)))
      (write-frame out fr))
    (define/public (queue-frames frs)
      (for ([fr (in-list frs)]) (queue-frame fr)))
    (define/public (flush-frames) (flush-output out))

    (define/public (connection-error errorcode [comment #f] #:debug [debug #""])
      (log-http2-debug "connection-error ~s, ~s" errorcode comment)
      (queue-frame (frame type:GOAWAY 0 0 (fp:goaway last-server-streamid errorcode debug)))
      (flush-frames)
      (set-closed! 'by-error)
      (for ([(streamid stream) (in-hash stream-table)])
        (send stream handle-ua-connection-error errorcode comment))
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
             (connection-error error:PROTOCOL_ERROR "reference to stream 0")]
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
      (and (not is-closed?) (make-stream (+ last-client-streamid 2) req send-req?)))

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
        [(? frame? fr)
         (handle-frame fr)
         (after-handle-frame)]
        ['EOF
         (set-closed! 'EOF)
         (close-input-port in)
         (close-output-port out)
         (for ([(streamid stream) (in-hash stream-table)])
           (send stream handle-eof))]
        [(? procedure?) (v)]))

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
               [_ (connection-error error:PROTOCOL_ERROR "expected continuation frame")])]
            [else (handle-frame* fr)]))

    (define/private (handle-multipart-frames frs)
      (define (get-header streamid first-headerbf)
        (define rest-headerbfs
          (for/list ([fr (in-list (cdr frs))])
            (fp:continuation-headerbf (frame-payload fr))))
        (define headerb (apply bytes-append first-headerbf rest-headerbfs))
        (with-handler (lambda (e)
                        (eprintf "********************\n")
                        ((error-display-handler) (exn-message e) e)
                        (eprintf "headerb = ~v\n" headerb)
                        (eprintf "********************\n")
                        (send (get-stream streamid) set-received! 'yes)
                        (connection-error error:COMPRESSION_ERROR "error decoding header"))
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
           (unless (= streamid 0) (connection-error error:PROTOCOL_ERROR "requires stream 0")))
         (define (check-stream-nonzero)
           (when (= streamid 0) (connection-error error:PROTOCOL_ERROR "reference to stream 0")))
         (match type
           [(== type:DATA)
            (check-stream-nonzero)
            (adjust-in-flow-window (- (frame-fc-length fr)))
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
            ;; If last-streamid=2^31-1 and errorcode = NO_ERROR, it is probably
            ;; a timeout warning, and it will (proabably) be followed by another
            ;; GOAWAY with a more specific last-streamid.
            ;; Note: GOAWAY does not close existing streams! (<= last-streamid)
            (for ([(streamid stream) (in-hash stream-table)]
                  #:when (> streamid last-streamid))
              (send stream handle-goaway last-streamid errorcode debug))
            (set-closed! 'by-goaway)]
           [(== type:WINDOW_UPDATE)
            (match-define (fp:window_update increment) payload)
            (when (zero? increment) (connection-error error:PROTOCOL_ERROR "zero increment"))
            (cond [(zero? streamid) (handle-connection-window_update flags increment)]
                  [else (send (stream) handle-window_update flags increment)])]
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

    (define/private (handle-connection-window_update flags delta)
      (adjust-out-flow-window delta))

    (define/private (after-handle-frame)
      (define target-in-flow-window (get-target-in-flow-window))
      (when (< in-flow-window target-in-flow-window)
        (define delta (- target-in-flow-window in-flow-window))
        (define max-frame-size (hash-ref my-config 'max-frame-size))
        ;; Only increase window when the difference w/ target is significant.
        (when (or (< (* 2 max-frame-size) delta)
                  (< (* 2 in-flow-window) target-in-flow-window))
          (queue-frame (frame type:WINDOW_UPDATE 0 0 (fp:window_update delta)))
          (adjust-in-flow-window delta))))

    ;; ------------------------------------------------------------
    ;; Flow control

    ;; The out-flow window is determined by the server.

    ;; The in-flow window targets a constant. See after-handle-frame.
    (define/private (get-target-in-flow-window) INIT-TARGET-IN-FLOW-WINDOW)

    ;; Out-flow window is decreased by queue-frame (on DATA send) and
    ;; increased by handle-connection-window_update.
    (define/private (adjust-out-flow-window delta)
      ;; FIXME: detect negative, detect over bound
      (set! out-flow-window (+ out-flow-window delta))
      (unless (< out-flow-window FLOW-WINDOW-BOUND)
        (connection-error error:FLOW_CONTROL_ERROR "window too large")))

    ;; In-flow window is decreased by handle-frame* (DATA case),
    ;; increased by after-handle-frame.
    (define/private (adjust-in-flow-window delta)
      (set! in-flow-window (+ in-flow-window delta))
      (unless (< in-flow-window FLOW-WINDOW-BOUND)
        (connection-error error:FLOW_CONTROL_ERROR "window too large")))

    ;; ============================================================
    ;; Connection threads (2 per connection)

    ;; Manager thread
    ;; - receives frames from reader thread, handles
    ;; - receives work from streams/users, handles
    ;; - must not call any user-supplied procedures, or block on any
    ;;   ports, except okay to write to out (?)
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
      (define manager-bored-evt
        (wrap-evt (if #f
                      (guard-evt
                       (lambda ()
                         (alarm-evt (+ (current-inexact-milliseconds) #e5e3))))
                      never-evt)
                  (lambda (ignored) (log-http2-debug "manager is bored!"))))
      ;; ----
      (define (loop/streams-changed)
        (define work-evts
          (for/list ([stream (in-hash-values stream-table)])
            (send stream get-work-evt)))
        (log-http2-debug "manager updating work evts (~s)" (length work-evts))
        (define streams-evt (apply choice-evt work-evts))
        (loop streams-evt))
      (define (loop streams-evt)
        (with-handlers ([(lambda (e) (eq? e 'escape-without-error)) void]
                        [(lambda (e) (eq? e 'stream-error)) void])
          (sync streams-evt
                reader-evt
                manager-bored-evt))
        (flush-frames)
        (if (begin0 streams-changed? (set! streams-changed? #f))
            (loop/streams-changed)
            (loop streams-evt)))
      (with-handlers ([(lambda (e) (eq? e 'connection-error))
                       (lambda (e) (log-http2-debug "manager stopped due to connection error"))])
        (loop/streams-changed)))

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
    ;; Methods called from user thread

    (define/public (open-request req)
      (define streambxe (make-box-evt))
      (define (do-open-request)
        (with-handler (lambda (e)
                        (box-evt-set! streambxe (lambda () (raise e)))
                        (raise e))
          (define stream (new-client-stream req #t))
          (box-evt-set! streambxe (lambda () stream))))
      (thread-send manager-thread do-open-request
                   (lambda () (box-evt-set! streambxe (lambda () #f))))
      (define get-stream (sync streambxe))
      ;;(define stream ((sync streambxe)))
      (define stream (get-stream))
      (cond [stream
             (define-values (pump-data-out resp-bxe)
               (send stream get-user-communication))
             (pump-data-out)
             resp-bxe]
            [else #f]))

    (define/public (register-user-abort-request stream)
      (define (do-register-abort) (send stream handle-user-abort))
      (thread-send manager-thread do-register-abort void))

    (define/public (abandon)
      (define (do-abandon)
        (define payload (fp:goaway last-server-streamid error:NO_ERROR #""))
        (queue-frame (frame type:GOAWAY 0 0 payload))
        (flush-frames)
        ;; Don't close/abandon output port, because may need to write eg
        ;; WINDOW_UPDATE frames to finish receiving from existing streams.
        (void))
      (unless is-closed?
        (set-closed! 'user-abandoned)
        (thread-send manager-thread do-abandon void)))

    ;; ========================================

    (define/private (send-handshake)
      (write-bytes http2-client-preface out)
      (define my-new-config my-config) ;; FIXME?
      (queue-frame (frame type:SETTINGS 0 0 (fp:settings null)))
      (set! my-configs/awaiting-ack (list my-new-config))
      (flush-frames))

    (send-handshake)

    ))
