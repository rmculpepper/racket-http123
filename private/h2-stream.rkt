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

    (define/public (client-originated?) (odd? streamid))
    (define/public (server-originated?) (even? streamid))
    (define/public (originator) (if (client-originated?) 'client 'server))

    ;; ============================================================
    ;; Stream Layer 1

    ;; This layer cares about the state machine described in Section 5. It does
    ;; not deal with the structure of HTTP requests.
    ;; - flow windows

    (define/private (connection-error errorcode [debug #""])
      (send conn connection-error errorcode debug))

    (define/private (stream-error errorcode #:raise [e #f])
      (queue-frame (frame type:RST_STREAM 0 streamid (fp:rst_stream errorcode)))
      (s2:error e))

    ;; ----------------------------------------
    ;; Flow windows

    ;; Stream-local limit on flow-controlled {sends, receives}.
    (define out-flow-window INIT-FLOW-WINDOW)
    (define in-flow-window INIT-FLOW-WINDOW)

    (define/public (get-effective-out-flow-window)
      (min out-flow-window (send conn get-out-flow-window)))

    (define/public (adjust-in-flow-window delta)
      (set! in-flow-window (+ in-flow-window delta))
      ;; FIXME: check for negative window error
      (send conn adjust-in-flow-window delta))

    ;; ----------------------------------------
    ;; Finite state machine

    ;; A StreamState is one of:
    ;; - 'idle                  S C
    ;; - 'reserved/local        S
    ;; - 'reserved/remote         C
    ;; - 'open                  S C
    ;; - 'half-closed/remote    S C
    ;; - 'half-closed/local     S C
    ;; - 'closed                S C -- actually, split close into 4 kinds

    (field [state 'idle]) ;; StreamState

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
        ;; Many flavors of closed state...
        [(closed/by-peer-rst)
         (stream-error error:STREAM_CLOSED)]
        [(closed/by-peer-end)
         (connection-error error:STREAM_CLOSED)]  ;; Why connection error?
        [(closed/by-me-recently)
         ;; Must ignore frames received in this state.
         (raise 'escape-without-error)]))

    (define/private (check-send-state transition)
      ;; Section 5.1
      (define (my-error)
        (error 'http2 "internal error: bad send state tx\n  state: ~s\n  tx: ~e"
               state transition))
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
           [(rst_stream) (set-state! 'closed)]
           [(window_update) (void)]
           [else (my-error)])]
        [(open)
         (case transition
           [(end_stream) (set-state! 'half-closed/local)]
           [(rst_stream) (set-state! 'closed)]
           ;; RFC says "any type"
           [(data headers push_promise window_update) (void)]
           [else (error 'check-state "internal error s1: transition = ~e" transition)])]
        [(half-closed/local)
         (case transition
           [(window_update) (void)]
           [(rst_stream) (set-state! 'closed)]
           [else (error 'check-state "internal error s2: transition = ~e" transition)])]
        [(half-closed/remote)
         (case transition
           [(rst_stream) (set-state! 'closed)]
           ;; RFC says "any type"
           [(data headers push_promise window_update) (void)]
           [else (stream-error error:STREAM_CLOSED)])]
        ;; Many flavors of closed state...
        [(closed/by-peer-rst) (my-error)]
        [(closed/by-peer-end) (my-error)]
        [(closed/by-me-recently) (my-error)]))

    (define/private (set-state! new-state)
      (define old-state state)
      (set! state new-state)
      (when (and (state:closed? new-state) (not (state:closed? old-state)))
        ;; Remove this connection's contribution to global target in-flow-window.
        (send conn adjust-target-in-flow-window (max 0 (- in-flow-window)))
        (set! in-flow-window 0))
      (s2:set-state! old-state new-state))

    (define/private (state:closed? state)
      (case state
        [(closed/by-peer-rst closed/by-peer-end closed/by-me-recently) #t]
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
      (s2:handle-data data end?))

    (define/public (handle-headers-payload flags payload)
      (match-define (fp:headers padlen _streamdep _weight hs) payload)
      (check-state 'headers)
      (define end? (flags-has? flags flag:END_STREAM))
      (when end? (check-state 'end_stream))
      (s2:handle-headers hs end?))

    (define/public (handle-priority-payload flags payload)
      (match-define (fp:priority streamdep weight) payload)
      ;; Allowed in any state
      (void))

    (define/public (handle-push_promise-payload flags payload)
      (match-define (fp:push_promise padlen promised-streamid headers) payload)
      (check-state 'push_promise)
      (s2:handle-push_promise promised-streamid headers))

    (define/public (handle-rst_stream errorcode)
      (check-state 'rst_stream)
      (s2:handle-rst_stream errorcode))

    (define/public (handle-goaway last-streamid errorcode debug)
      (check-state 'rst_stream) ;; pretend
      (s2:handle-goaway last-streamid errorcode debug))

    (define/public (handle-window_update flags increment)
      (set! out-flow-window (+ out-flow-window increment))
      (unless (< out-flow-window FLOW-WINDOW-BOUND)
        (stream-error error:FLOW_CONTROL_ERROR)))

    ;; ----------------------------------------
    ;; Sending frames to the server

    (define/private (queue-frame fr)
      (match (frame-type fr)
        [(== type:DATA)
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
         (check-send-state 'window_update)]
        [_ (void)])
      ;; FIXME: avoid sending multiple RST_STREAM frames, etc
      (send conn queue-frame fr))

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

    (define/public (s2:set-state! old-state1 new-state1)
      (void))

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
           [(headers+end)       (set-s2-state! 'complete)]
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

    (define/public (teardown:before-request) (void))

    (define/public (setup:sending-request-data)
      (set! s2-work-evt
            (handle-evt (guard-evt
                         (lambda ()
                           (cond [(port-closed? in-from-user)
                                  ;; We read an EOF (user-out is closed), so
                                  ;; in-from-user is always ready now; ignore.
                                  never-evt]
                                 [(not (positive? (get-effective-out-flow-window)))
                                  ;; Only check if out-flow window is nonempty.
                                  never-evt]
                                 [else in-from-user])))
                        (lambda (ignored) (send-request-data-from-user)))))

    (define/public (teardown:sending-request-data)
      (close-input-port in-from-user)
      (set! s2-work-evt never-evt))

    (define/public (setup:before-response) (void))
    (define/public (teardown:before-response) (void))

    (define/public (setup:reading-response-data)
      (set! info-for-exn (hash-set info-for-exn 'received 'yes))
      (set! s2-work-evt
            (handle-evt (guard-evt (lambda () user-in-last-progress-evt))
                        (lambda (ignored) (update-target-in-flow-window)))))

    (define/public (teardown:reading-response-data)
      (set! s2-work-evt never-evt))

    (define/public (setup:done)
      (set! s2-work-evt
            (handle-evt (alarm-evt (+ (current-inexact-milliseconds) KEEP-AFTER-CLOSE-MS))
                        (lambda (ignore)
                          (log-http2-debug "removing closed stream #~s" streamid)
                          (send conn remove-stream streamid)))))

    ;; ----------------------------------------
    ;; Initiate request

    (define/private (initiate-request req send-req?)
      (cond [send-req?
             (match-define (request method url headers data) req)
             (log-http2-debug "#~s initiating ~s request" streamid method)
             ;; FIXME: for http2, data must be #f or bytes or 'delayed
             (define more-headers (make-http2-headers method url))
             (define enc-headers (encode-headers (append more-headers headers)
                                                 (send conn get-sending-dt)))
             ;; FIXME: split into frames if necessary
             (define no-content?
               (or (eq? data #f)
                   (and (bytes? data) (zero? (bytes-length data)))))
             (queue-frame
              (frame type:HEADERS
                     (+ flag:END_HEADERS (if no-content? flag:END_STREAM 0))
                     streamid
                     (fp:headers 0 0 0 enc-headers)))
             ;; FIXME: if small data, send immediately (must check out-flow-window, though)
             (if no-content?
                 (check-s2-state 'user-request+end)
                 (check-s2-state 'user-request))]
            [else
             (check-s2-state 'user-request+end)]))

    ;; FIXME: pseudo-headers MUST appear first, contiguous
    ;; FIXME: 8.1.2.2 forbids Connection header, others
    (define/private (make-http2-headers method u)
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

    ;; The manager thread syncs on this event to check if async work is
    ;; available to be done on this stream (and to do it).
    ;; Determined by on s2-state.
    (define s2-work-evt never-evt)  ;; Evt[Void]

    (define/public (get-work-evt)
      ;; Use guard-evt so manager automatically gets state changes without
      ;; having to rescan all streams / etc.
      (guard-evt (lambda () s2-work-evt)))

    (define info-for-exn
      (hasheq 'version 'http/2
              'request req
              'streamid streamid
              'received 'unknown))

    ;; Stage 1. Sending request data
    (define-values (in-from-user user-out) (make-pipe))
    ;; Stage 2. Receive response header
    (define resp-header-bxe (make-box-evt #t))  ;; can be used to send raised-exn back
    ;; Stage 3. Reading response data
    ;; Want to tie flow control to user's consumption of data. That is, when
    ;; user consumes N bytes, want to request another N bytes from server.
    (define-values (user-in out-to-user raise-user-exn) (make-wrapped-pipe))
    (define user-in-last-position (file-position user-in))
    (define user-in-last-progress-evt (port-progress-evt user-in)) ;; progress or close!

    (define/public (get-user-communication)
      (values user-out
              resp-header-bxe
              user-in))

    (define/private (send-exn-to-user e)
      (box-evt-set! resp-header-bxe (lambda () (raise e)))
      (raise-user-exn e))

    ;; ----------------------------------------
    ;; Handling frames from server

    (define/public (s2:handle-data data end?)
      (write-bytes data out-to-user)
      (when end? (close-output-port out-to-user))
      (check-s2-state (if end? 'data+end 'data)))

    (define/public (s2:handle-headers headers end?)
      (log-http2-debug "#~s returning headers to user" streamid)
      (box-evt-set! resp-header-bxe (lambda () headers))
      (check-s2-state (if end? 'headers+end 'headers)))

    (define/public (s2:handle-push_promise promised-streamid headers)
      (send conn handle-push_promise promised-streamid headers))

    (define/public (s2:handle-rst_stream errorcode)
      (send-exn-to-user
       (exn:fail:http123 "stream closed by server"
                         (current-continuation-marks)
                         (hash-set* info-for-exn 'code 'RST_STREAM)))
      (set-s2-state! 'done))

    (define/public (s2:handle-goaway last-streamid errorcode debug)
      ;; If this streamid > last-streamid, then server has not processed
      ;; this request, and it's okay to auto-retry on new connection.
      (when (> streamid last-streamid)
        (set! info-for-exn (hash-set info-for-exn 'received 'no)))
      (send-exn-to-user
       (exn:fail:http123 "connection closed by server"
                         (current-continuation-marks)
                         (hash-set* info-for-exn 'code 'GOAWAY)))
      (set-s2-state! 'done))

    (define/public (s2:error e)
      (send-exn-to-user e)
      (set-s2-state! 'done)
      (raise 'stream-error))

    ;; ----------------------------------------
    ;; Sending request data from user

    (define/private (send-request-data-from-user)
      ;; PRE: in-from-user is ready for input
      (define (check-at-eof?) ;; check for EOF w/o blocking
        (eof-object? (peek-bytes-avail!* (make-bytes 1) 0 #f in-from-user)))
      (define (send-data data end?)
        (queue-frame (frame type:DATA (if end? flag:END_STREAM 0) streamid data))
        ;; On EOF, close in-from-user so evt will ignore it.
        (when end? (close-input-port in-from-user)))
      (define len (min (get-effective-out-flow-window)
                       (get-max-data-payload-length)))
      ((with-handlers ([exn? (lambda (e) (abort-request-data) (lambda () (void)))])
         (define buf (make-bytes len))
         (define r (read-bytes-avail!* buf in-from-user))
         (cond [(eof-object? r) (lambda () (send-data #"" #t))]
               [(< r len) (lambda () (send-data (subbytes buf 0 r) (check-at-eof?)))]
               [else (lambda () (send-data buf (check-at-eof?)))]))))

    (define/private (abort-request-data)
      (queue-frame (frame type:RST_STREAM 0 null (fp:rst_stream error:CANCEL)))
      (set-s2-state! 'done))

    (define/private (get-max-data-payload-length)
      (min (send conn get-config-value 'max-frame-size)
           ;; FIXME: make my preference for max frame size configurable
           (expt 2 20)))

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
             (set! in-flow-window (+ in-flow-window delta))
             (queue-frame (frame type:WINDOW_UPDATE 0 streamid (fp:window_update delta)))
             (send conn adjust-target-flow-window delta)]))

    (define/private (update-user-in-mark/get-delta)
      ;; FIXME: get file-position and port-progress-evt atomically ??
      (define position (file-position user-in))
      (define progress-evt (port-progress-evt user-in))
      (define delta (- position user-in-last-position))
      (set! user-in-last-position position)
      (set! user-in-last-progress-evt progress-evt)
      delta)

    ;; ============================================================
    (initiate-request req send-req?)
    ))
