;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/class
         racket/match
         binaryio/reader
         scramble/evt
         "interfaces.rkt"
         "hpack.rkt"
         "h2-frame.rkt"
         "h2-stream.rkt"
         "h2-client-app.rkt")
(provide (all-defined-out))

;; FIXME/TODO:
;; - move timeout state to separate object?
;; - add way(s) to shut down connection, including ports
;;   - abandon waits for server to send EOF
;;   - user could use custodian
;; - configuration
;;   - various limits
;;   - flow-control (eg, init in-flow window)
;;   - timeouts
;; - handle CONNECT ?
;; - per stream timeouts:
;;   - total time limit: if full response not received within S seconds, abort
;;   - time since recv: if no bytes received in last S seconds, abort
;;   - minimum throughput limit: if fewer than N bytes received in last S seconds, abort

;; References:
;; - https://tools.ietf.org/html/rfc7540

(define http2-alpn-protocol #"h2")
(define http2-client-preface #"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n")

(define INIT-TARGET-IN-FLOW-WINDOW (expt 2 20)) ;; FIXME: config?

;; FIXME: config? ok defaults?
(define TIMEOUT-RECV-NONE-MS 10e3) ;; time after recv to send goaway when no open streams
(define TIMEOUT-RECV-STREAMS-MS 10e3) ;; time after recv to send ping when open streams
(define TIMEOUT-PING-MS 20e3) ;; time after recv to send goaway when open streams
(define TIMEOUT-CLOSED-MS 2e3) ;; time after sending goaway to close ports

(define standard-init-config
  (hasheq 'header-table-size 4096
          'enable-push 1
          'max-concurrent-streams +inf.0
          'initial-window-size INIT-FLOW-WINDOW
          'max-frame-size DEFAULT-MAX-FRAME-SIZE
          'max-header-list-size +inf.0))

(define init-config
  (hash-set* standard-init-config
             'enable-push 0))

;; ============================================================

(define counter 0)

(define http2-endpoint-base%
  (class* object% ()
    (init-field in out)
    (super-new)

    (field [ID (string-upcase (format "#~X" (begin0 counter (set! counter (add1 counter)))))])

    (field [br (make-binary-reader in)])

    (define peer-config standard-init-config)
    (define my-config standard-init-config) ;; acked by server
    (define my-configs/awaiting-ack null)   ;; oldest-first

    (define/public (get-ID) ID)
    (define/public (get-config) peer-config)
    (define/public (get-my-config) my-config)

    (define/public (get-config-value key)
      (hash-ref peer-config key))

    ;; Connection-wide limit on flow-controlled {sends, receives}.
    ;; Flow control is only applied to DATA frames (see 6.9).
    (define out-flow-window INIT-FLOW-WINDOW)
    (define in-flow-window INIT-FLOW-WINDOW)

    ;; FIXME: resize on SETTINGS receive or ack?
    (define reading-dt (make-dtable (get-config-value 'header-table-size)))
    (define sending-dt (make-dtable (get-config-value 'header-table-size)))

    (define/public (get-sending-dt) sending-dt)

    ;; is-closed? : (U #f 'by-goaway 'by-error 'user-abandoned 'EOF)
    ;; Value is #f if allowed to start new streams; symbol if not allowed.
    (define is-closed? #f)
    (define/public (open?) (not is-closed?))
    (define/public (closed?) (not (open?)))

    (define/private (set-closed! reason [timeout? #t])
      (unless is-closed?
        (set! is-closed? reason)
        (when timeout? (start-close-timeout!))))

    (define/private (close-ports)
      (break-thread reader-thread)
      (close-input-port in)
      (close-output-port out)
      (log-http2-debug "~a closed ports and stopped reader thread" ID))

    ;; ----------------------------------------

    (define/public (queue-frame fr)
      (log-http2-debug "~a --> ~a" ID
                       (parameterize ((error-print-width 60))
                         (format "~e" fr)))
      (adjust-out-flow-window (- (frame-fc-length fr)))
      (write-frame out fr))
    (define/public (queue-frames frs)
      (for ([fr (in-list frs)]) (queue-frame fr)))
    (define/public (flush-frames)
      (unless (port-closed? out)
        (flush-output out)))

    (define/public (connection-error errorcode [comment #f] #:debug [debug #""])
      (log-http2-debug "~a connection-error ~s, ~s" ID errorcode comment)
      (queue-frame (frame type:GOAWAY 0 0 (fp:goaway last-server-streamid errorcode debug)))
      (flush-frames)
      (set-closed! 'by-error)
      (for ([(streamid stream) (in-hash stream-table)])
        (send stream handle-ua-connection-error errorcode comment))
      (raise 'connection-error))

    (define/public (send-settings my-new-config)
      (define settings
        (for/list ([(key val) (in-hash my-new-config)]
                   #:when (and (hash-has-key? standard-init-config key)
                               (not (equal? (hash-ref standard-init-config key) val))))
          (setting (encode-setting-key key) val)))
      (queue-frame (frame type:SETTINGS 0 0 (fp:settings settings)))
      (set! my-configs/awaiting-ack (list my-new-config))
      (flush-frames))

    ;; ----------------------------------------

    ;; stream-table : Hash[Nat => stream%]
    ;; If a streamid has no entry in stream-table and it is less than
    ;; or equal to last-{s,c}-streamid, it is closed.
    (define stream-table (make-hasheqv))
    (define streams-changed? #f)
    (field [last-server-streamid 0]) ;; Nat -- last streamid used by server
    (field [last-client-streamid 1]) ;; Nat -- last streamid used by client

    (define/public (get-stream streamid [fail-ok? #f])
      (cond [(zero? streamid)
             (connection-error error:PROTOCOL_ERROR "reference to stream 0")]
            [(hash-ref stream-table streamid #f)
             => values]
            [fail-ok? #f]
            [else (connection-error error:STREAM_CLOSED "unknown stream")]))

    (define/public (remove-stream streamid)
      (set! streams-changed? #t)
      (hash-remove! stream-table streamid))

    (define/public (make-stream streamid app)
      (define stream (new http2-stream% (conn this) (streamid streamid) (app app)))
      (set! streams-changed? #t)
      (hash-set! stream-table streamid stream)
      (cond [(streamid-from-client? streamid)
             (set! last-client-streamid streamid)]
            [(streamid-from-server? streamid)
             (set! last-server-streamid streamid)])
      stream)

    (define/public (new-client-stream app)
      (make-stream (+ last-client-streamid 2) app))

    ;; ----------------------------------------
    ;; Handling frames received from peer

    (define/private (handle-frame-or-other v)
      (match v
        [(? frame? fr)
         (update-received-time!)
         (handle-frame fr)
         (after-handle-frame)]
        ['EOF
         (set-closed! 'EOF)
         (close-ports)
         (for ([(streamid stream) (in-hash stream-table)])
           (send stream handle-eof))]
        ['frame-size-error
         (connection-error error:FRAME_SIZE_ERROR "frame size error")]
        ['frame-bad-padding
         (connection-error error:PROTOCOL_ERROR "bad padding")]
        [(? procedure?) (v)]))

    (define in-continue-frames null) ;; (Listof Frame), reversed

    (define/private (handle-frame fr)
      (define (bad comment) (connection-error error:PROTOCOL_ERROR comment))
      (match fr
        [(frame type flags streamid payload)
         (cond [(or (= type type:HEADERS) (= type type:PUSH_PROMISE))
                (unless (null? in-continue-frames) (bad "expected CONTINUATION frame"))
                (cond [(flags-has? flags flag:END_HEADERS)
                       (handle-multipart-frames (list fr))]
                      [else (set! in-continue-frames (list fr))])]
               [(= type type:CONTINUATION)
                ;; PRE: in-continue-frames != null
                (unless (pair? in-continue-frames) (bad "unexpected CONTINUATION frame"))
                (unless (= streamid (frame-streamid (car in-continue-frames)))
                  (bad "CONTINUATION frame has wrong streamid"))
                (cond [(flags-has? flags flag:END_HEADERS)
                       (define frs (reverse (cons fr in-continue-frames)))
                       (set! in-continue-frames null)
                       (handle-multipart-frames frs)]
                      [else (set! in-continue-frames (cons fr in-continue-frames))])]
               [else
                (unless (null? in-continue-frames) (bad "expected CONTINUATION frame"))
                (handle-frame* fr)])]))

    (define/private (handle-multipart-frames frs)
      (define (get-header streamid first-headerbf)
        (define rest-headerbfs
          (for/list ([fr (in-list (cdr frs))])
            (fp:continuation-headerbf (frame-payload fr))))
        (define headerb (apply bytes-append first-headerbf rest-headerbfs))
        (with-handler (lambda (e)
                        (let ([stream (get-stream streamid #t)])
                          (when stream (send stream set-received! 'yes)))
                        (connection-error error:COMPRESSION_ERROR "error decoding header"))
          (decode-header headerb reading-dt)))
      (match (car frs)
        [(frame (== type:HEADERS) flags streamid
                (fp:headers padlen streamdep weight headerbf))
         (define header (get-header streamid headerbf))
         (define payload (fp:headers padlen streamdep weight header))
         (handle-frame* (frame type:HEADERS flags streamid payload))]
        [(frame (== type:PUSH_PROMISE) flags streamid
                (fp:push_promise padlen promised-streamid headerbf))
         (define header (get-header streamid headerbf))
         (define payload (fp:push_promise padlen promised-streamid header))
         (handle-frame* (frame type:PUSH_PROMISE flags streamid payload))]))

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
            (send (stream) handle-headers-payload flags payload)]
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
                     (connection-error error:FRAME_SIZE_ERROR "non-empty SETTINGS ack"))
                   (unless (pair? my-configs/awaiting-ack)
                     (connection-error error:PROTOCOL_ERROR "unexpected SETTINGS ack"))
                   (set! my-config (car my-configs/awaiting-ack))
                   (set! my-configs/awaiting-ack (cdr my-configs/awaiting-ack))]
                  [else (handle-settings settings)])]
           [(== type:PUSH_PROMISE)
            (check-stream-nonzero)
            (when (zero? (hash-ref my-config 'enable-push))
              (connection-error error:PROTOCOL_ERROR "push not enabled"))
            (send (stream) handle-push_promise-payload flags payload)]
           [(== type:PING)
            (check-stream-zero)
            ;; FIXME: move to public method to allow override?
            (unless (flags-has? flags flag:ACK)
              (queue-frame (frame type:PING flag:ACK 0 payload))
              (flush-frames))]
           [(== type:GOAWAY)
            (check-stream-zero)
            (match-define (fp:goaway last-streamid errorcode debug) payload)
            ;; If last-streamid=2^31-1 and errorcode = NO_ERROR, it is probably
            ;; a timeout warning, and it will (probably) be followed by another
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
            (connection-error error:PROTOCOL_ERROR "unexpected CONTINUATION frame")]
           [_
            ;; Ignore unknown frames, per ??.
            (void)])]))

    (define/private (handle-settings settings)
      (define new-config
        (for/fold ([h peer-config]) ([s (in-list settings)])
          (match-define (setting key value) s)
          (case key
            [(enable-push)
             (unless (memv value '(0 1))
               (connection-error error:PROTOCOL_ERROR "bad enable_push value"))]
            [(initial-window-size)
             (unless (< value FLOW-WINDOW-BOUND)
               (connection-error error:FLOW_CONTROL_ERROR "window too large"))
             (define delta (- value (hash-ref h 'initial-window-size)))
             (for ([(streamid stream) (in-hash stream-table)])
               (send stream adjust-out-flow-window delta))])
          (hash-set h key value)))
      (set! peer-config new-config)
      (queue-frame (frame type:SETTINGS flag:ACK 0 (fp:settings null))))

    (define/private (handle-connection-window_update flags delta)
      (adjust-out-flow-window delta))

    (define/private (after-handle-frame)
      (define target-in-flow-window (get-target-in-flow-window))
      (when (< in-flow-window target-in-flow-window)
        (define delta (- target-in-flow-window in-flow-window))
        (define max-frame-size (hash-ref my-config 'max-frame-size))
        ;; Only increase window when the difference w/ target is significant.
        (when (or (>= delta (* 2 max-frame-size))
                  (< (* 2 in-flow-window) target-in-flow-window))
          (queue-frame (frame type:WINDOW_UPDATE 0 0 (fp:window_update delta)))
          (adjust-in-flow-window delta))))

    ;; ------------------------------------------------------------
    ;; Flow control

    ;; The out-flow window is determined by the peer.

    (define/public (get-out-flow-window) out-flow-window)

    ;; The in-flow window targets a constant. See after-handle-frame.
    (define/private (get-target-in-flow-window) INIT-TARGET-IN-FLOW-WINDOW)

    ;; Out-flow window is decreased by queue-frame (on DATA send) and
    ;; increased by handle-connection-window_update.
    (define/private (adjust-out-flow-window delta)
      ;; FIXME: detect negative
      (set! out-flow-window (+ out-flow-window delta))
      (when (positive? delta)   ;; avoids loop w/ queue-frame
        (unless (< out-flow-window FLOW-WINDOW-BOUND)
          (connection-error error:FLOW_CONTROL_ERROR "window too large"))))

    ;; In-flow window is decreased by handle-frame* (DATA case),
    ;; increased by after-handle-frame.
    (define/private (adjust-in-flow-window delta)
      (set! in-flow-window (+ in-flow-window delta))
      (unless (< in-flow-window FLOW-WINDOW-BOUND)
        (connection-error error:FLOW_CONTROL_ERROR "window too large")))

    ;; ------------------------------------------------------------
    ;; Timeouts

    ;; timeout-mode is one of
    ;; - 'recv   : last received a frame at timeout-base-ms
    ;; - 'ping   : sent a ping at timeout-base-ms, not yet ack'd
    ;; - 'closed : sent GOAWAY frame at timeout-base-ms, waiting to close ports
    (define timeout-mode 'recv)
    (define timeout-base-ms (current-milliseconds))

    (define/private (start-close-timeout!)
      (set! timeout-mode 'closed)
      (set! timeout-base-ms (current-milliseconds)))

    (define/private (update-received-time!)
      (case timeout-mode
        [(recv ping)
         (set! timeout-base-ms (current-milliseconds))
         (unless (eq? timeout-mode 'recv)
           (set! timeout-mode 'recv))]
        [(closed) (void)]))

    (define/private (get-timeout-evt)
      (define mode timeout-mode)
      (wrap-evt (alarm-evt
                 (+ timeout-base-ms
                    (case mode
                      [(recv)
                       (if (zero? (hash-count stream-table))
                           TIMEOUT-RECV-NONE-MS
                           TIMEOUT-RECV-STREAMS-MS)]
                      [(ping)
                       TIMEOUT-PING-MS]
                      [(closed)
                       TIMEOUT-CLOSED-MS])))
                (lambda (_) (handle-timeout mode))))

    (define/private (handle-timeout mode)
      (case mode
        [(recv)
         (cond [(zero? (hash-count stream-table))
                (log-http2-debug "~a timeout, abandoning connection" ID)
                (set-closed! 'timeout)
                (send-goaway)]
               [else
                (log-http2-debug "~a timeout, sending PING" ID)
                (queue-frame (frame type:PING 0 0 (fp:ping (make-bytes 8 0))))
                (set! timeout-mode 'ping)])]
        [(ping)
         (log-http2-debug "~a timeout (no response to PING), abandoning connection" ID)
         (set-closed! 'timeout)
         (send-goaway)]
        [(closed)
         (close-ports)
         (for ([stream (in-hash-values stream-table)])
           (send stream handle-timeout))]))

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

    (define/public (start-manager)
      (with-handlers ([(lambda (e) (eq? e 'connection-error))
                       (lambda (e)
                         (log-http2-debug "~a manager stopped due to connection error" ID))]
                      [(lambda (e) #t)
                       (lambda (e)
                         (log-http2-debug "~a manager stopped due to uncaught exn: ~e" ID e)
                         (when (exn? e) ((error-display-handler) (exn-message e) e)))])
        (manager)))

    (define/private (manager)
      (define reader-evt
        (handle-evt (thread-receive-evt)
                    (lambda (tre)
                      (define fr (thread-receive))
                      (handle-frame-or-other fr))))
      (define timeout-evt (guard-evt (lambda () (get-timeout-evt))))
      ;; ----
      (define (loop/streams-changed)
        (define work-evts
          (for/list ([stream (in-hash-values stream-table)])
            (send stream get-work-evt)))
        (log-http2-debug "~a manager updating work evts (~s)" ID (length work-evts))
        (define streams-evt (apply choice-evt work-evts))
        (loop streams-evt))
      (define (loop streams-evt)
        (with-handlers ([(lambda (e) (eq? e 'escape-without-error)) void]
                        [(lambda (e) (eq? e 'stream-error)) void])
          (sync streams-evt
                reader-evt
                timeout-evt))
        (flush-frames)
        (cond [(port-closed? out)
               (log-http2-debug "~a manager stopped; closed" ID)]
              [(begin0 streams-changed? (set! streams-changed? #f))
               (loop/streams-changed)]
              [else (loop streams-evt)]))
      ;; ----
      (loop/streams-changed))

    (define/public (start-reader)
      (with-handlers ([exn:break? void]) (reader)))

    (define/private (reader)
      (sync (port-closed-evt in) in) ;; wait for input or EOF or closed
      (cond [(port-closed? in)
             (log-http2-debug "~a <-- closed" ID)
             (thread-send manager-thread 'EOF void) ;; treat like EOF
             (void)]
            [(with-handlers ([exn:fail? (lambda (e) #t)])
               ;; There is a race between the port-closed? guard and peek-byte,
               ;; so catch the occasional "input port is closed" error.
               (eof-object? (peek-byte in)))
             (log-http2-debug "~a <-- EOF" ID)
             (thread-send manager-thread 'EOF void)
             (void)]
            [else
             (define fr
               (with-handler (lambda (e)
                               (thread-send manager-thread 'EOF void)
                               (raise e))
                 (read-frame br (get-config-value 'max-frame-size))))
             (log-http2-debug "~a <-- ~a" ID
                              (parameterize ((error-print-width 60))
                                (format "~e" fr)))
             (thread-send manager-thread fr void)
             (cond [(frame? fr) (reader)]
                   [else (thread-send manager-thread 'EOF void)])]))

    (define manager-thread (thread (lambda () (start-manager))))
    (define reader-thread (thread (lambda () (start-reader))))

    (define/public (send-to-manager v [fail void])
      (thread-send manager-thread v fail))

    ;; ============================================================
    ;; Methods called from user thread

    (define/public (abandon)
      (unless is-closed?
        (set-closed! 'user-abandoned #f)
        (send-to-manager
         (lambda ()
           (start-close-timeout!)
           (send-goaway)))))

    ;; ============================================================
    ;; Hello, Goodbye

    (define/private (send-goaway)
      (define payload (fp:goaway last-server-streamid error:NO_ERROR #""))
      (queue-frame (frame type:GOAWAY 0 0 payload))
      ;; Don't close/abandon output port, because may need to write eg
      ;; WINDOW_UPDATE frames to finish receiving from existing streams.
      (void))
    ))


;; ============================================================

(define http2-actual-connection%
  (class* http2-endpoint-base% (http-actual-connection<%>)
    (init parent [my-new-config init-config])
    (inherit-field out)
    (inherit new-client-stream
             send-to-manager
             send-settings)
    (super-new)

    ;; ============================================================
    ;; Methods called from user thread

    (define/public (open-request req)
      (define bxe (make-box-evt))
      (define (do-open-request)
        (with-handler (lambda (e)
                        (box-evt-set! bxe (lambda () (raise e)))
                        (raise e))
          (define app (new h2-client-stream-app% (req req)))
          (define stream (new-client-stream app))
          (box-evt-set! bxe (lambda () app))))
      (send-to-manager do-open-request
                       (lambda () (box-evt-set! bxe (lambda () #f))))
      (match ((sync bxe))
        [(? values app)
         (define-values (pump-data-out resp-bxe)
           (send app get-user-communication))
         (pump-data-out)
         resp-bxe]
        [_ #f]))

    (define/public (register-user-abort-request stream e)
      (define (do-register-abort) (send stream handle-user-abort e))
      (send-to-manager do-register-abort))

    ;; ============================================================
    ;; Hello, Goodbye

    (define/private (send-handshake my-new-config)
      (write-bytes http2-client-preface out)
      (send-settings my-new-config))

    (send-handshake my-new-config)
    ))
