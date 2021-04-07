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
;; - https://tools.ietf.org/html/rfc7540

(define http2-alpn-protocol #"h2")
(define http2-client-preface #"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n")

(define FLOW-CONTROL-BOUND (expt 2 31))
(define INIT-FLOW-WINDOW (sub1 (expt 2 16)))

(define init-config
  (hasheq 'header-table-size 4096
          'enable-push 1
          'max-concurrent-streams +inf.0
          'initial-window-size (sub1 (expt 2 16))
          'max-frame-size (expt 2 14)
          'max-header-list-size +inf.0))

(define http2-actual-connection%
  (class* object% ()
    (init-field in out)
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

    ;; ----------------------------------------

    (define/public (send-frames frs [flush? #t])
      (for ([fr (in-list frs)]) (write-frame out fr))
      (when flush? (flush-output out)))
    (define/public (send-frame fr) (send-frames (list fr)))
    (define/public (queue-frame fr) (queue-frames (list fr)))
    (define/public (queue-frames frs) (send-frames frs #f))
    (define/public (flush-frames) (send-frames null #t))

    (define/public (connection-error errorcode)
      '___)

    ;; ----------------------------------------

    ;; stream-table : Hash[Nat => stream%]
    ;; If a streamid has no entry in stream-table and it is less than
    ;; or equal to last-{s,c}-streamid, it is closed.
    (define stream-table (make-hasheqv))
    (define last-server-streamid 0) ;; Nat -- last streamid used by server
    (define last-client-streamid 0) ;; Nat -- last streamid used by client

    (define/public (get-stream streamid)
      (cond [(zero? streamid)
             (error 'get-stream "invalid streamid: zero")]
            [(hash-ref stream-table streamid #f)
             => values]
            [(or (and (streamid-from-client? streamid)
                      (<= streamid last-client-streamid))
                 (and (streamid-from-server? streamid)
                      (<= streamid last-server-streamid)))
             the-closed-stream]
            [(streamid-from-server? streamid)
             (make-stream streamid #f #f)]
            [fail-ok? #f]
            [else (error 'get-stream "no such client stream: ~e" streamid)])) ;; FIXME

    (define/public (new-client-stream req send-req?)
      (make-stream (+ last-client-streamid 2) req send-req?))

    (define/public (make-stream streamid req send-req?)
      (define stream
        (new http2-stream% (conn this) (streamid streamid) (req req) (send-req? send-req?)))
      (hash-set! stream-table streamid stream)
      (cond [(streamid-from-client? streamid)
             (set! last-client-streamid streamid)]
            [(streamid-from-server? streamid)
             (set! last-server-streamid streamid)])
      stream)

    ;; ----------------------------------------
    ;; Handling frames received from server

    (define in-continue-frames null) ;; (Listof Frame), reversed

    (define/public (handle-frame fr)
      (cond [(pair? in-continue-frames)
             (define streamid (frame-streamid (car in-continue-frames)))
             (match fr
               [(frame (== type:CONTINUATION) flags (== streamid) payload)
                (cond [(flags-has? flags flag:END_HEADERS)
                       (define frs (reverse (cons fr in-continue-frames)))
                       (set! in-continue-frames null)
                       (handle-headers-frames frs)]
                      [else (set! in-continue-frames (cons fr in-continue-frames))])]
               [_ (connection-error error:PROTOCOL_ERROR)])]
            [else (handle-frame* fr)]))

    (define/public (handle-header-frames frs)
      (define rest-headerbfs
        (for/list ([fr (in-list frs)])
          (fp:continuation-headerbf (frame-payload fr))))
      (match (car frs)
        [(frame (== type:HEADERS) flags streamid
                (fp:headers padlen streamdep weight headerbf))
         (define headerbf* (apply bytes-append headerbf rest-headerbfs))
         (define headers (decode-headers headerbf* reading-dt))
         (send (get-stream streamid) handle-headers-payload
               flags (fp:headers padlen streamdep weight headers))]
        [(frame (== type:PUSH_PROMISE) flags streamid
                (fp:push_promise padlen promised-streamid headerbf))
         (define headerbf* (apply bytes-append headerbf rest-headerbfs))
         (define headers (decode-headers headerbf* reading-dt))
         (send (get-stream streamid) handle-push_promise-payload
               flags (fp:push_promise padlen promised-streamid headers))]))

    (define/public (handle-frame* fr)
      (match fr
        [(frame type flags streamid payload)
         (define (stream) (get-stream streamid))
         (define (check-stream-zero)
           (unless (= streamid 0) (connection-error error:PROTOCOL_ERROR)))
         (define (check-stream-nonzero)
           (when (= streamid 0) (connection-error error:PROTOCOL_ERROR)))
         (match type
           [(== type:DATA)
            (check-stream-nonzero)
            (send (stream) handle-data-payload flags payload)]
           [(== type:HEADERS)
            (check-stream-nonzero)
            (cond [(flags-has? flags flag:END_HEADERS)
                   (handle-headers-frames (list fr))]
                  [else (set! in-continue-frames (list fr))])]
           [(== type:PRIORITY)
            (check-stream-nonzero)
            (send (stream) handle-priority-payload payload)]
           [(== type:RST_STREAM)
            (check-stream-nonzero)
            (send (stream) handle-rst_stream-payload flags payload)]
           [(== type:SETTINGS)
            (check-stream-zero)
            (match-define (fp:settings settings) payload)
            (cond [(flags-has? flags flag:ACK)
                   (unless (null? settings)
                     (connection-error error:FRAME_SIZE_ERROR))
                   (unless (pair? my-configs/awaiting-ack)
                     (connection-error error:PROTOCOL_ERROR))
                   (set! my-config (car my-configs/awaiting-ack))
                   (set! my-configs/awaiting-ack (cdr my-configs/awaiting-ack))]
                  [else (handle-settings)])]
           [(== type:PUSH_PROMISE)
            (check-stream-nonzero)
            (when (zero? (hash-ref my-config 'enable-push))
              (connection-error error:PROTOCOL_ERROR))
            (cond [(flags-has? flags flag:END_HEADERS)
                   (handle-headers-frames (list fr))]
                  [else (set! in-continue-frames (list fr))])]
           [(== type:PING)
            (check-stream-zero)
            (send-frame (frame type:PING flag:ACK 0 payload))]
           [(== type:GOAWAY)
            (check-stream-zero)
            (match-define (fp:goaway last-streamid errorcode debug) payload)
            ;; FIXME
            '___]
           [(== type:WINDOW_UPDATE)
            (match-define (fp:window_update increment) payload)
            (when (zero? increment) (connection-error error:PROTOCOL_ERROR))
            (cond [(zero? streamid)
                   (handle-connection-window-update flags increment)]
                  [else
                   (send (stream) handle-window-update flags increment)])]
           [(== type:CONTINUATION)
            (connection-error error:PROTOCOL_ERROR)]
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
               (connection-error error:PROTOCOL_ERROR))]
            [(initial-window-size)
             ;; FIXME: changes current stream windows???
             (unless (< value FLOW-WINDOW-BOUND)
               (connection-error error:FLOW_CONTROL_ERROR))])
          (hash-set h key value)))
      (set! config new-config)
      (send-frame (frame type:SETTINGS flag:ACK) 0 (fp:settings null)))

    (define/public (handle-connection-window-update flags increment)
      (set! out-flow-window (+ out-flow-window increment))
      (unless (< out-flow-window FLOW-CONTROL-BOUND)
        (connection-error error:FLOW_CONTROL_ERROR)))

    (define/public (adjust-in-flow-window increment)
      (set! in-flow-window (+ in-flow-window)))

    (define/public (adjust-target-flow-window increment)
      ;; FIXME: sending window_update frame is delayed until... ???
      (set! target-in-flow-window (+ target-in-flow-window increment)))

    (define/public (after-handle-frame)
      (when (< in-flow-window target-in-flow-window)
        (define diff (- target-in-flow-window in-flow-window))
        (queue-frame (frame type:UPDATE_WINDOW 0 0 (fp:update_window diff)))
        (set! in-flow-window target-in-flow-window))
      (flush-frames))

    ;; ============================================================
    ;; Connection threads (2 per connection)

    ;; Manager thread
    ;; - receives frames from reader thread, handles
    ;; - receives work from streams/users, handles
    ;; Reader thread
    ;; - just reads frames from input, sends to manager

    ;; FIXME: make kill-safe

    (define/private (manager)
      (define reader-evt
        (handle-evt (thread-receive-evt)
                    (lambda (tre)
                      (define fr (thread-receive))
                      (with-handlers ([exn?
                                       (lambda (e)
                                         ((error-display-handler) (exn-message e) e))])
                        (handle-frame fr)))))
      (let streamsloop ()
        (define streams-evt
          (apply choice-evt
                 (for/list ([stream (in-hash-values stream-table)])
                   (send stream get-work-evt))))
        (define saved-last-server-id last-server-streamid)
        (define saved-last-client-id last-client-streamid)
        (let loop ()
          (sync streams-evt reader-evt)
          (if (and (= last-server-streamid saved-last-server-id)
                   (= last-client-streamid saved-last-client-id))
              (loop)
              (streamsloop)))))

    (define/private (reader)
      (define fr (read-frame br))
      ;; FIXME: handle reading errors...
      (eprintf "<< ~e\n" fr)
      (thread-send manager-thread fr)
      (reader))

    (define manager-thread (thread (lambda () (manager))))
    (define reader-thread (thread (lambda () (reader))))

    ;; ========================================

    ;; called by user thread
    (define/public (open-request req)
      (define stream (new-client-stream req #t))
      ;; Stream automatically sends request headers.
      (define-values (user-out resp-header-bxe user-in)
        (send stream get-user-communication))
      ;; User thread writes request content.
      (match (request-data req)
        [(? bytes? data)
         (write-bytes data user-out)
         (close-output-port user-out)]
        [(? procedure? put-data)
         (put-data (lambda (data) (write-bytes data user-out)))
         (close-output-port user-out)]
        [#f (close-output-port user-out)])
      ;; Get response header. (Note: may receive raised-exception instead!)
      (define resp-header (sync resp-header-bxe))
      ;; ----
      (define code (cond [(assoc #":status" resp-headers) => cdr] [else #f]))
      (response2 resp-header user-in))

    ))

;; ========================================

(define http2-stream-layer1%
  (class* object% ()
    (init-field conn streamid)
    (super-new)

    (define/public (client-originated?) (odd? streamid))
    (define/public (server-originated?) (even? streamid))
    (define/public (originator) (if (client-originated?) 'client 'server))

    ;; ============================================================
    ;; Stream Layer 1

    ;; This layer cares about the state machine described in Section 5. It does
    ;; not deal with the structure of HTTP requests.
    ;; - flow windows

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
           [else (connection-error error:PROTOCOL_ERROR)])]
        [(reserved/local) ;; Only for servers
         (case transition
           [(rst_stream) (set-state! 'closed)]
           [(window_update) (void)]
           [else (connection-error error:PROTOCOL_ERROR)])]
        [(reserved/remote)
         (case transition
           [(headers) (set-state! 'half-closed/local)]
           [(rst_stream) (set-state! 'closed)]
           [else (connection-error error:PROTCOL_ERROR)])]
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
         (escape-without-error)]
        [(closed/by-me-old)  ;; FIXME: represent with different object instead?
         ;; connection vs stream error unspecified
         (connection-error error:STREAM_CLOSED)]))

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
        [(closed/by-me-recently) (my-error)]
        [(closed/by-me-old) (my-error)]))

    (define/private (set-state! new-state)
      (define old-state state)
      (set! state new-state)
      (when (and (state:closed? new-state) (not (state:closed? old-state)))
        ;; Remove this connection's contribution to global target in-flow-window.
        (send conn adjust-target-in-flow-window (max 0 (- in-flow-window)))
        (set! in-flow-window 0))
      (s2:set-state! old-state new-state))

    ;; ----------------------------------------
    ;; Handling frames from server

    (define/public (handle-data-payload flags payload)
      (match-define (fp:data padlen data) payload)
      (check-state 'data)
      (let ([len (payload-length flags payload)])
        (adjust-in-flow-window (- len)))
      (define end-stream? (flag-has? flags flag:END_STREAM))
      (when end-stream? (check-state 'end_stream))
      (s2:handle-data data end?))

    (define/public (handle-headers-payload flags payload)
      (match-define (fp:headers padlen _streamdep _weight hs) payload)
      (check-state 'headers)
      (define end-stream? (flag-has? flags flag:END_STREAM))
      (when end-stream? (check-state 'end_stream))
      (s2:handle-headers hs end?))

    (define/public (handle-priority-payload flags payload)
      (match-define (fp:priority streamdep weight) payload)
      ;; Allowed in any state
      (void))

    (define/public (handle-push_promise-payload flags payload)
      (match-define (fp:push_promise padlen promised-streamid headers) payload)
      (check-state 'push_promise)
      (s2:handle-push_promise promised-streamid headers))

    (define/public (handle-rst_stream-payload flags payload)
      (match-define (fp:rst_stream errorcode) payload)
      (check-state 'rst_stream)
      (s2:handle-rst_stream errcode))

    (define/public (handle-window-update flags increment)
      (set! out-flow-window (+ out-flow-window increment))
      (unless (< out-flow-window FLOW-CONTROL-BOUND)
        (stream-error error:FLOW_CONTROL_ERROR)))

    ;; ----------------------------------------
    ;; Sending frames to the server

    (define/private (queue-frame fr)
      ;; FIXME: check/change state
      (check-send-state ...)
      ;; FIXME: avoid sending multiple RST_STREAM frames, etc
      (send conn queue-frame))
    ))

(define http2-stream-layer2%
  (class* http2-stream-layer1% ()
    (inherit-field conn streamid)
    (super-new)

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
      (set! s2-work-evt
            (handle-evt (guard-evt (lambda () user-in-last-progress-evt))
                        (lambda (ignored) (update-target-in-flow-window)))))

    (define/public (teardown:reading-response-data)
      (set! s2-work-evt never-evt))

    (define/public (setup:done) (void))

    ;; ----------------------------------------

    ;; The manager thread syncs on this event to check if async work is
    ;; available to be done on this stream (and to do it).
    ;; Determined by on s2-state.
    (define s2-work-evt never-evt)  ;; Evt[Void]

    (define/public (get-work-evt) s2-work-evt)

    ;; Stage 1. Request
    (init-field req)
    ;; Stage 2. Sending request data
    (define-values (in-from-user user-out) (make-pipe))
    ;; Stage 4. Receive response header
    (define resp-header-bxe (make-box-evt #t))  ;; can be used to send raised-exn back
    ;; Stage 3. Reading response data
    ;; Want to tie flow control to user's consumption of data. That is, when
    ;; user consumes N bytes, want to request another N bytes from server.
    (define-values (user-in out-to-user) (make-pipe))
    (define wrapped-user-in (wrap-input-port user-in))
    (define user-in-last-position (file-position user-in))
    (define user-in-last-progress-evt (port-progress-evt user-in)) ;; progress or close!

    (define/public (get-user-communication)
      (values user-out
              resp-header-bxe
              user-in))

    ;; ----------------------------------------
    ;; Initiate request

    (define/public (initiate-request)
      ...req...
      (if ...
          (check-s2-state 'user-request)
          (check-s2-state 'user-request+end)))

    ;; ----------------------------------------
    ;; Handling frames from server

    (define/public (s2:handle-data data end?)
      (write-bytes data out-to-user)
      (when end? (close-output-port out-to-user))
      (check-s2-state (if end? 'data+end 'data)))

    (define/public (s2:handle-headers headers end?)
      (box-evt-set! resp-header-bxe (lambda () headers))
      (check-s2-state (if end? 'headers+end 'headers)))

    (define/public (s2:handle-push_promise promised-streamid headers)
      (send conn handle-push_promise promised-streamid headers))

    (define/public (s2:handle-rst_stream errcode)
      (set-s2-state! 'done))

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
      (define buf (make-bytes len))
      (define r (read-bytes-avail!* buf in-from-user))
      (cond [(eof-object? r) (send-data #"" #t)]
            [(< r len) (send-data (subbytes buf 0 r) (check-at-eof?))]
            [else (send-data buf (check-at-eof?))]))

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
             (queue-frame (frame type:RST_FRAME 0 streamid (fp:rst_stream error:CANCEL)))
             (check-s2-state 'end)]
            [else ;; want increment more data
             (define delta (update-user-in-mark/get-delta))
             (set! in-flow-window (+ in-flow-window delta))
             (queue-frame (frame type:WINDOW_UPDATE 0 streamid (fp:window_update delta)))
             (send conn adjust-target-flow-window delta)]))

    (define/privae (update-user-in-mark/get-delta)
      ;; FIXME: get file-position and port-progress-evt atomically ??
      (define position (file-position user-in))
      (define progress-evt (port-progress-evt user-in))
      (define delta (- position user-in-last-position))
      (set! user-in-last-position position)
      (set! user-in-last-progress-evt user-in-progress-evt)
      delta)

    ))

;; ----------------------------------------

(define the-closed-stream
  '___)

;; ============================================================

#|
(struct sync-vector (vec sema [unlocked #:mutable])
(define (make-sync-vector n)
  (sync-vector (make-vector n #f) (make-semaphore 0) 0))
(define (sync-vector-add! sv k v) ;; must add in order from 0
  (match-define (sync-vector vec sema _) sv)
  (vector-set! vec k v)
  (semaphore-post sema))
(define (sync-vector-ref sv k)
  (match-define (sync-vector vec sema unlocked) sv)
  (let loop ([unlocked unlocked])
    (if (< k unlocked)
        (vector-ref vec k)
        (begin (semaphore-wait sema)
               (set-sync-vector-unlocked! sv (add1 unlocked))
               (loop (add1 unlocked))))))
|#

(define-syntax (define-sync stx)
  (syntax-case stx ()
    [(_ name)
     (with-syntax ([getter (format-id #'name "get-~a" #'name)]
                   [install (format-id #'name "install-~a!" #'name)])
       #'(begin (define tmp-field #f)
                (define tmp-sema (make-semaphore 0))
                (define tmp-peek (semaphore-peek-evt tmp-sema))
                (define-syntax name (syntax-rules ()))
                (define/public (getter) (sync tmp-peek) tmp-field)
                (define/public (install v) (set! tmp-field v) (semaphore-post tmp-sema))))]))

(define http2-stream-user%
  (class* object% ()
    (init-field req)
    (super-new)

    (define-sync send-request-headers)
    (define-sync request-data-out)
    (define-sync response-headers)
    (define-sync response-data-in)
    ;; FIXME: what about push_promises ??
    ))

(define (perform-request req)
  (define u (...get-user-object...))
  ((send su get-send-req-headers) ...req-headers...)
  (define data-out (send su get-request-data-out))
  ...write data to data-out, then close port...
  (define resp-headers (send su get-resp-headers))
  (define response-data-in (send su get-response-data-in))
  ...read from response-data-in...
  ...)

;; on request, StreamUser
;;   0.  sends request headers
;;   1.  sends data (maybe)
;;   2.  receives response headers
;;   3.  receives data (via inputport)
;;   4.  (maybe) receives push_promises, new Streams and StreamUser objects

;; on push_promise, StreamUser
;;   0'. receives synthetic request headers
;;   2.  receives response headers
;;   3.  ...same...
