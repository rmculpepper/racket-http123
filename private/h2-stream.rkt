#lang racket/base
(require racket/class
         racket/match
         racket/promise
         net/url-string
         "interfaces.rkt"
         "header.rkt"
         "response.rkt"
         "io.rkt"
         "request.rkt"
         "h2-frame.rkt"
         "h2-pack.rkt")
(provide (all-defined-out))

(define INIT-FLOW-WINDOW (sub1 (expt 2 16)))
(define FLOW-WINDOW-BOUND (expt 2 31))

(define KEEP-AFTER-CLOSE-MS (* 2 1000.0)) ;; keep closed stream for 2s

(define http2-stream%
  (class* object% ()
    (init-field conn streamid)
    (init req send-req?)
    (super-new)

    ;; Stream-local limit on flow-controlled {sends, receives}.
    (define out-flow-window INIT-FLOW-WINDOW)
    (define in-flow-window INIT-FLOW-WINDOW)

    (field [state 'idle]) ;; StreamState, see below
    (field [s2 (new stream-level2% (s1 this) (streamid streamid) (conn conn)
                    (req req) (send-req? send-req?))])

    (define/public (client-originated?) (odd? streamid))
    (define/public (server-originated?) (even? streamid))
    (define/public (originator) (if (client-originated?) 'client 'server))

    ;; ============================================================
    ;; Methods forwarded to Stream Layer 2

    (define/public (get-user-communication)
      (send s2 get-user-communication))
    (define/public (get-work-evt)
      (send s2 get-work-evt))

    ;; ============================================================
    ;; Stream Layer 1

    ;; This layer cares about the state machine described in Section 5. It does
    ;; not deal with the structure of HTTP requests.
    ;; - flow windows

    (define/private (connection-error errorcode [debug #""])
      (send conn connection-error errorcode debug))

    (define/private (stream-error errorcode #:message [msg #f] #:raise [e #f])
      (log-http2-debug "stream error, code = ~s, message = ~e" errorcode msg)
      (queue-frame (frame type:RST_STREAM 0 streamid (fp:rst_stream errorcode)))
      (send s2 signal-error errorcode msg e))

    ;; ----------------------------------------
    ;; Flow windows

    (define/public (get-effective-out-flow-window)
      (min out-flow-window (send conn get-out-flow-window)))

    ;; Increased by queue-frame (WINDOW_UPDATE case), decreased by
    ;; handle-data-payload.
    (define/public (adjust-in-flow-window delta)
      ;; FIXME: check bounds errors
      (set! in-flow-window (+ in-flow-window delta)))

    ;; Increased by handle-window_update, decreased by queue-frame (DATA case).
    (define/private (adjust-out-flow-window delta)
      (set! out-flow-window (+ out-flow-window delta))
      (unless (< out-flow-window FLOW-WINDOW-BOUND)
        (stream-error error:FLOW_CONTROL_ERROR)))

    ;; ----------------------------------------
    ;; Finite state machine

    ;; A StreamState is one of:
    ;; - 'idle                  S C
    ;; - 'reserved/local        S
    ;; - 'reserved/remote         C
    ;; - 'open                  S C
    ;; - 'half-closed/remote    S C
    ;; - 'half-closed/local     S C
    ;; - 'closed/by-me-recently S C -- server might not know closed yet
    ;; - 'closed                S C -- server knows closed

    (define/private (check-state transition)
      ;; Section 5.1
      ;; A (receive) transition is either a frame type (except 'priority,
      ;; allowed anywhere) or 'end_stream.
      (case state
        [(idle)
         (case transition
           [(headers) (set-state! 'open)]
           [else (connection-error error:PROTOCOL_ERROR "bad idle state tx")])]
        [(reserved/local) ;; Only for servers
         (case transition
           [(rst_stream) (set-state! 'closed)]
           [(window_update) (void)]
           [else (connection-error error:PROTOCOL_ERROR "bad reserved/local tx")])]
        [(reserved/remote)
         (case transition
           [(headers) (set-state! 'half-closed/local)]
           [(rst_stream) (set-state! 'closed)]
           [else (connection-error error:PROTOCOL_ERROR "bad reserved/remote tx")])]
        [(open)
         (case transition
           [(end_stream) (set-state! 'half-closed/remote)]
           [(rst_stream) (set-state! 'closed)]
           ;; RFC says "any type"
           [(data headers push_promise window_update) (void)]
           [else (error 'check-state "internal error 1: transition = ~e" transition)])]
        [(half-closed/local)
         (case transition
           [(end_stream) (set-state! 'closed)]
           [(rst_stream) (set-state! 'closed)]
           ;; RFC says "any type"
           [(data headers push_promise window_update) (void)]
           [else (error 'check-state "internal error 2: transition = ~e" transition)])]
        [(half-closed/remote)
         (case transition
           [(rst_stream) (set-state! 'closed)]
           [(window_update) (void)]
           [else (stream-error error:STREAM_CLOSED)])]
        [(closed/by-me-recently)
         ;; Must ignore frames received in this state.
         (raise 'escape-without-error)]))

    (define/private (check-send-state transition)
      ;; Section 5.1
      (define (my-error)
        (define fmt "internal error: bad send state transition\n  state: ~e\n  transition: ~e")
        (define msg (format fmt state transition))
        (stream-error error:INTERNAL_ERROR #:message msg))
      (case state
        [(idle)
         (case transition
           [(headers) (set-state! 'open)]
           [else (my-error)])]
        [(reserved/local) ;; Only for servers
         (case transition
           [(headers) (set-state! 'half-closed/remote)]
           [(rst_stream) (set-state! 'closed)]
           [else (my-error)])]
        [(reserved/remote)
         (case transition
           [(rst_stream) (set-state! 'closed/by-me-recently)]
           [(window_update) (void)]
           [else (my-error)])]
        [(open)
         (case transition
           [(end_stream) (set-state! 'half-closed/local)]
           [(rst_stream) (set-state! 'closed/by-me-recently)]
           ;; RFC says "any type"
           [(data headers push_promise window_update) (void)]
           [else (error 'check-state "internal error s1: transition = ~e" transition)])]
        [(half-closed/local)
         (case transition
           [(window_update) (void)]
           [(rst_stream) (set-state! 'closed/by-me-recently)]
           [else (error 'check-state "internal error s2: transition = ~e" transition)])]
        [(half-closed/remote)
         (case transition
           [(rst_stream) (set-state! 'closed/by-me-recently)]
           ;; RFC says "any type"
           [(data headers push_promise window_update) (void)]
           [else (stream-error error:STREAM_CLOSED)])]
        [(closed closed/by-me-recently) (my-error)]))

    (define/private (set-state! new-state)
      (define old-state state)
      (set! state new-state)
      (when (eq? new-state 'closed)
        (log-http2-debug "CLOSING NOW")
        (send conn remove-stream streamid)))

    (define/private (state:closed? state)
      (case state
        [(closed closed/by-me-recently) #t]
        [else #f]))

    ;; ----------------------------------------
    ;; Handling frames from server

    (define/public (handle-data-payload flags payload)
      (match-define (fp:data padlen data) payload)
      (check-state 'data)
      (let ([len (payload-length flags payload)])
        (adjust-in-flow-window (- len)))
      (define end? (flags-has? flags flag:END_STREAM))
      (when end? (check-state 'end_stream))
      (send s2 handle-data data end?))

    (define/public (handle-headers-payload flags payload)
      (match-define (fp:headers padlen _streamdep _weight hs) payload)
      (check-state 'headers)
      (define end? (flags-has? flags flag:END_STREAM))
      (when end? (check-state 'end_stream))
      (send s2 handle-headers hs end?))

    (define/public (handle-priority-payload flags payload)
      (match-define (fp:priority streamdep weight) payload)
      ;; Allowed in any state
      (void))

    (define/public (handle-push_promise-payload flags payload)
      (match-define (fp:push_promise padlen promised-streamid header) payload)
      (check-state 'push_promise)
      (send s2 handle-push_promise promised-streamid header))

    (define/public (handle-rst_stream errorcode)
      (check-state 'rst_stream)
      (send s2 handle-rst_stream errorcode))

    (define/public (handle-goaway last-streamid errorcode debug)
      (check-state 'rst_stream) ;; pretend
      (send s2 handle-goaway last-streamid errorcode debug))

    (define/public (handle-window_update flags delta)
      (adjust-out-flow-window delta))

    (define/public (handle-user-abort)
      ;; FIXME: raise better exn?
      (stream-error error:CANCEL))

    ;; ----------------------------------------
    ;; Sending frames to the server

    (define/public (queue-frame fr)
      (match (frame-type fr)
        [(== type:DATA)
         (adjust-out-flow-window (- (frame-fc-length fr)))
         (check-send-state 'data)
         (when (frame-has-flag? fr flag:END_STREAM)
           (check-send-state 'end_stream))]
        [(== type:HEADERS)
         (check-send-state 'headers)
         (when (frame-has-flag? fr flag:END_STREAM)
           (check-send-state 'end_stream))]
        [(== type:RST_STREAM)
         (check-send-state 'rst_stream)]
        [(== type:WINDOW_UPDATE)
         (adjust-in-flow-window (fp:window_update-increment (frame-payload fr)))
         (check-send-state 'window_update)]
        [_ (void)])
      ;; FIXME: avoid sending multiple RST_STREAM frames, etc
      (send conn queue-frame fr))

    (define/public (queue-frames frs)
      (for ([fr (in-list frs)]) (queue-frame fr)))
    ))

;; ============================================================

(define stream-level2%
  (class object%
    (init-field s1 streamid conn req)
    (init send-req?)
    (super-new)

    ;; ============================================================
    ;; Methods forwarded to Stream Layer 1

    (define/public (queue-frame fr) (send s1 queue-frame fr))
    (define/public (queue-frames frs) (send s1 queue-frames frs))

    ;; ============================================================
    ;; Stream Layer 2

    ;; This layer cares about HTTP and communicating with the user.

    ;; An S2State is one of
    ;; - 'before-request            -- none
    ;; - 'sending-request-data      -- pump in-from-user to server (mod flow-window)
    ;; - 'before-response           -- none
    ;; - 'reading-response-data     -- pump server data to out-to-user (adj flow-window)
    ;; - 'done                      -- whether successful or not

    (define s2-state 'before-request)   ;; S2State

    (define/public (check-s2-state transition)
      ;; A transition does not include push_promise or rst_stream.
      (define (bad) (error 'check-s2-state "bad transition: ~e" transition)) ;; FIXME
      (case s2-state
        [(before-request)
         (case transition
           [(user-request)      (set-s2-state! 'sending-request-data)]
           [(user-request+end)  (set-s2-state! 'before-response)]
           [else (bad)])]
        [(sending-request-data)
         (case transition
           [(end)               (set-s2-state! 'before-response)]
           [else (bad)])]
        [(before-response)
         (case transition
           [(headers)           (set-s2-state! 'reading-response-data)]
           [(headers+end)       (set-s2-state! 'done)]
           [else (bad)])]
        [(reading-response-data)
         (case transition
           [(data)              (void)]
           [(end data+end)      (set-s2-state! 'done)]
           [else (bad)])]
        [(done)
         (void)]))

    (define/public (set-s2-state! new-s2-state)
      (define old-s2-state s2-state)
      (set! s2-state new-s2-state)
      (log-http2-debug "#~a changed state -> ~a" streamid new-s2-state)
      (case old-s2-state
        [(before-request)        (teardown:before-request)]
        [(sending-request-data)  (teardown:sending-request-data)]
        [(before-response)       (teardown:before-response)]
        [(reading-response-data) (teardown:reading-response-data)])
      (case new-s2-state
        [(sending-request-data)  (setup:sending-request-data)]
        [(before-response)       (setup:before-response)]
        [(reading-response-data) (setup:reading-response-data)]
        [(done)                  (setup:done)]))

    (define/public (teardown:before-request)
      (set-work-evt! never-evt))

    (define/public (setup:sending-request-data)
      (set-work-evt!
       (handle-evt (guard-evt
                    (lambda ()
                      (cond [(port-closed? in-from-user)
                             ;; We read an EOF (user-out is closed), so
                             ;; in-from-user is always ready now; ignore.
                             never-evt]
                            [(not (positive? (send s1 get-effective-out-flow-window)))
                             ;; Only check if out-flow window is nonempty.
                             never-evt]
                            [else in-from-user])))
                   (lambda (ignored) (send-request-data-from-user)))))

    (define/public (teardown:sending-request-data)
      (close-input-port in-from-user)
      (set-work-evt! never-evt))

    (define/public (setup:before-response) (void))
    (define/public (teardown:before-response) (void))

    (define/public (setup:reading-response-data)
      (set-work-evt!
       (handle-evt (guard-evt (lambda () user-in-last-progress-evt))
                   (lambda (ignored) (update-target-in-flow-window)))))

    (define/public (teardown:reading-response-data)
      (set-work-evt! never-evt))

    (define/public (setup:done)
      (set-work-evt!
       (handle-evt (alarm-evt (+ (current-inexact-milliseconds) KEEP-AFTER-CLOSE-MS))
                   (lambda (ignore)
                     (log-http2-debug "removing closed stream #~s" streamid)
                     (send conn remove-stream streamid)))))

    ;; ----------------------------------------

    ;; The manager thread syncs on this event to check if async work is
    ;; available to be done on this stream (and to do it).
    ;; Determined by on s2-state.
    (define s2-work-evt never-evt)  ;; Evt[Void]

    (define/private (set-work-evt! evt)
      (set! s2-work-evt evt))

    (define/public (get-work-evt)
      ;; Use guard-evt so manager automatically gets state changes without
      ;; having to rescan all streams / etc.
      (guard-evt (lambda () s2-work-evt)))

    (define info-for-exn
      (hasheq 'version 'http/2
              'request req
              'streamid streamid
              'received 'unknown))

    (define/private (set-received! received)
      (unless (eq? (hash-ref info-for-exn 'received) received)
        (set! info-for-exn (hash-set info-for-exn 'received received))))

    ;; Stage 1. Sending request data
    (define-values (in-from-user user-out) (make-pipe))
    ;; Stage 2. Receive response header
    (define resp-bxe (make-box-evt))  ;; can be used to send raised-exn back
    ;; Stage 3. Reading response data
    ;; Want to tie flow control to user's consumption of data. That is, when
    ;; user consumes N bytes, want to request another N bytes from server.
    (define-values (user-in out-to-user raise-user-exn) (make-wrapped-pipe))
    (define user-in-last-position (file-position user-in))
    (define user-in-last-progress-evt (port-progress-evt user-in)) ;; progress or close!
    ;; Stage 4.
    (define trailerbxe (make-box-evt))

    (define/public (get-user-communication)
      (values (make-pump-data-out) resp-bxe))

    (define/private (send-exn-to-user e)
      (box-evt-set! resp-bxe (lambda () (raise e)))
      (box-evt-set! trailerbxe (lambda () (raise e)))
      (raise-user-exn e))

    ;; ----------------------------------------
    ;; Handling frames from server

    (define/public (handle-data data end?)
      (set-received! 'yes)
      (write-bytes data out-to-user)
      (when end? (close-output-port out-to-user))
      (check-s2-state (if end? 'data+end 'data)))

    (define/public (handle-headers header end?)
      (set-received! 'yes)
      (log-http2-debug "#~s returning header to user" streamid)
      (box-evt-set! resp-bxe (make-response-thunk header))
      (check-s2-state (if end? 'headers+end 'headers)))

    (define/public (handle-push_promise promised-streamid header)
      (set-received! 'yes)
      (send conn handle-push_promise promised-streamid header))

    (define/public (handle-rst_stream errorcode)
      (send-exn-to-user
       (build-exn "stream closed by server"
                  (hash-set* info-for-exn 'code 'RST_STREAM)))
      (set-s2-state! 'done))

    (define/public (handle-goaway last-streamid errorcode debug)
      ;; If this streamid > last-streamid, then server has not processed
      ;; this request, and it's okay to auto-retry on new connection.
      (when (> streamid last-streamid)
        (set-received! 'no))
      (send-exn-to-user
       (build-exn "connection closed by server"
                  (hash-set* info-for-exn 'code 'GOAWAY)))
      (set-s2-state! 'done))

    (define/public (signal-error errorcode msg e)
      (send-exn-to-user (or e (make-stream-exn errorcode msg)))
      (set-s2-state! 'done)
      (raise 'stream-error))

    (define/private (make-stream-exn errorcode msg)
      (define code (or (decode-error-code errorcode) 'unknown-stream-error))
      (build-exn (or msg "stream closed by user agent")
                 (hash-set* info-for-exn 'code code 'http2-errorcode errorcode)))

    ;; ----------------------------------------
    ;; Send request

    ;; Must send the request immediately upon creating the stream, otherwise if
    ;; we create two streams and send the later streamid's request first, it
    ;; implicitly closes the earlier stream! (See HTTP/2 spec.)

    ;; Another alternative would be to delay allocating the streamid until
    ;; sending the request.

    (define/private (send-request req)
      (match-define (request method url header data) req)
      (log-http2-debug "#~s initiating ~s request" streamid method)
      (define pseudo-header (make-pseudo-header method url))
      (define enc-header (encode-header (append pseudo-header header)
                                        (send conn get-sending-dt)))
      (define no-content?
        (or (eq? data #f)
            (and (bytes? data) (zero? (bytes-length data)))))
      ;; FIXME: split into frames if necessary
      (queue-frames (make-header-frames enc-header no-content?))
      (if no-content?
          (check-s2-state 'user-request+end)
          (check-s2-state 'user-request)))

    (define/private (make-header-frames enc-header no-content?)
      (define len (bytes-length enc-header))
      (define frame-size (send conn get-config-value 'max-frame-size))
      ;; Note: if padding or priority flags set, must adjust frame-size!
      (for/list ([start (in-range 0 len frame-size)] [index (in-naturals)])
        (define end (min (+ start frame-size) len))
        (define flags
          (cond [(< end len) 0]
                [else (+ flag:END_HEADERS (if no-content? flag:END_STREAM 0))]))
        (define headerbf (subbytes enc-header start end))
        (if (zero? index)
            (frame type:HEADERS flags streamid (fp:headers 0 0 0 headerbf))
            (frame type:CONTINUATION flags streamid (fp:continuation headerbf)))))

    ;; FIXME: pseudo-header fields MUST appear first, contiguous
    ;; FIXME: 8.1.2.2 forbids Connection header, others
    (define/private (make-pseudo-header method u)
      (list (list #":method" (symbol->bytes method))
            (list #":scheme" (string->bytes/utf-8 (url-scheme u)))
            (list #":authority" (url-authority->bytes u)) ;; SHOULD use instead of Host
            (list #":path" (url-path/no-fragment->bytes u))))

    (define/private (url-path/no-fragment->bytes u)
      (string->bytes/utf-8
       (url->string (url #f #f #f #f (url-path-absolute? u) (url-path u) (url-query u) #f))))
    (define/private (url-authority->bytes u)
      (string->bytes/utf-8
       (if (url-port u) (format "~a:~a" (url-host u) (url-port u)) (url-host u))))

    ;; ----------------------------------------
    ;; Sending request data from user

    (define/private (make-pump-data-out)
      (define data (request-data req))
      (cond [(not data)
             (lambda ()
               (close-output-port user-out))]
            [(bytes? data)
             (lambda ()
               (write-bytes data user-out)
               (close-output-port user-out))]
            [else
             (lambda ()
               (with-handler (lambda (e)
                               (register-abort-request)
                               (raise e))
                 (call-with-continuation-barrier
                  (lambda ()
                    (data (lambda (data-bs) (write-bytes data-bs user-out)))
                    (close-output-port user-out)))))]))

    (define/private (register-abort-request) ;; called in user thread
      (send conn register-user-abort-request s1))

    ;; ----------------------------------------
    ;; Sending request data from user

    (define/private (send-request-data-from-user)
      ;; PRE: in-from-user is ready for input
      (define (check-at-eof?) ;; check for EOF w/o blocking
        (eof-object? (peek-bytes-avail!* (make-bytes 1) 0 #f in-from-user)))
      (define (send-data data end?)
        (queue-frame (frame type:DATA (if end? flag:END_STREAM 0) streamid data))
        ;; On EOF, close in-from-user so work-evt will ignore it.
        (when end? (close-input-port in-from-user)))
      (define allowed-len (send s1 get-effective-out-flow-window))
      (let loop ([allowed-len allowed-len])
        (define len (min allowed-len (get-max-data-payload-length)))
        (define buf (make-bytes len))
        (define r (read-bytes-avail!* buf in-from-user))
        (define end? (or (eof-object? r) (check-at-eof?)))
        (cond [(eof-object? r) (send-data #"" #t)]
              [(< r len) (send-data (subbytes buf 0 r) end?)]
              [else (send-data buf end?)])
        (unless end? (loop (- allowed-len r)))))

    (define/private (get-max-data-payload-length)
      (min (send conn get-config-value 'max-frame-size)
           ;; FIXME: make my preference for max frame size configurable
           (expt 2 20)))

    ;; ----------------------------------------
    ;; Make response

    (define/private (make-response-thunk header-entries)
      ;; Create a promise so that work happens in user thread.
      (define resp-promise
        (delay/sync
         (define header
           (with-handler (lambda (e)
                           (h2-error "error processing header"
                                     #:base-info info-for-exn
                                     #:info (hasheq 'wrapped-exn e)))
             (make-header-from-entries header-entries)))
         (unless (send header value-matches? ':status #rx#"[1-5][0-9][0-9]")
           (h2-error "bad or missing status from server"
                     #:base-info info-for-exn
                     #:info (hasheq 'code 'bad-status 'header header)))
         (define status (send header get-integer-value ':status))
         (send header remove! ':status)
         (new http2-response%
              (status-code status)
              (header header)
              (content user-in)
              (trailerbxe trailerbxe))))
      (lambda () (force resp-promise)))

    ;; ----------------------------------------
    ;; Updating flow window when user consumes data

    ;; called in response to progress/closure in `user-in` port
    (define/public (update-target-in-flow-window)
      (cond [(port-closed? user-in) ;; don't want more data
             (set! user-in-last-progress-evt never-evt)
             (queue-frame (frame type:RST_STREAM 0 streamid (fp:rst_stream error:CANCEL)))
             (check-s2-state 'end)]
            [else ;; want increment more data
             (define delta (update-user-in-mark/get-delta))
             (queue-frame (frame type:WINDOW_UPDATE 0 streamid (fp:window_update delta)))]))

    (define/private (update-user-in-mark/get-delta)
      ;; FIXME: get file-position and port-progress-evt atomically ??
      (define position (file-position user-in))
      (define progress-evt (port-progress-evt user-in))
      (define delta (- position user-in-last-position))
      (set! user-in-last-position position)
      (set! user-in-last-progress-evt progress-evt)
      delta)

    ;; ============================================================
    ;; Finish initialization

    (cond [send-req? (send-request req)]
          [else (check-s2-state 'user-request+end)])
    ))
