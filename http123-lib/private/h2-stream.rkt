;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/class
         racket/match
         "interfaces.rkt"
         "h2-frame.rkt")
(provide (all-defined-out))

(define INIT-FLOW-WINDOW (sub1 (expt 2 16)))
(define FLOW-WINDOW-BOUND (expt 2 31))

(define SIGNIFICANT-DELTA (expt 2 12))

(define KEEP-AFTER-CLOSE-MS (* 2 1000.0)) ;; keep closed stream for 2s

(define http2-stream%
  (class* object% ()
    (init-field conn streamid req)
    (init send-req?)
    (super-new)

    (field [ID (format "~a.~a" (send conn get-ID) streamid)])

    ;; This class cares about frames, the state machine described in Section 5,
    ;; and flow control. It contains a pstate object (protocol state) that deals
    ;; with the structure of HTTP requests.

    ;; ------------------------------------------------------------
    ;; Stream info

    (define/public (get-ID) ID)
    (define/public (get-conn) conn)
    (define/public (get-streamid) streamid)
    (define/public (client-originated?) (odd? streamid))
    (define/public (server-originated?) (even? streamid))
    (define/public (originator) (if (client-originated?) 'client 'server))

    ;; ------------------------------------------------------------
    ;; Flow control

    ;; Stream-local limit on flow-controlled {sends, receives}.
    (define out-flow-window INIT-FLOW-WINDOW)
    (define in-flow-window INIT-FLOW-WINDOW)
    (define in-flow-buffer-size INIT-FLOW-WINDOW)

    (define/public (get-effective-out-flow-window)
      (min out-flow-window (send conn get-out-flow-window)))

    ;; Increased by queue-frame (WINDOW_UPDATE case), decreased by
    ;; handle-data-payload.
    (define/public (adjust-in-flow-window delta)
      (set! in-flow-window (+ in-flow-window delta)))

    ;; Increased by handle-window_update and SETTINGS frame, decreased by
    ;; queue-frame (DATA case).
    (define/public (adjust-out-flow-window delta)
      (set! out-flow-window (+ out-flow-window delta))
      (when (positive? delta)
        ;; SETTINGS may make this go negative; not an error, see 6.9.2.
        (unless (< out-flow-window FLOW-WINDOW-BOUND)
          (stream-error error:FLOW_CONTROL_ERROR))))

    ;; Called from pstate in response to user reading from buffer.
    (define/public (update-in-flow-window buffered)
      ;; target-ifw  : space avail in buffer = max(0, bufsize - buffered)
      (define target-in-flow-window (max 0 (- in-flow-buffer-size buffered)))
      (when (< in-flow-window target-in-flow-window)
        (define delta (- target-in-flow-window in-flow-window))
        ;; Only increase window when the difference w/ target is significant.
        (when (or (>= delta SIGNIFICANT-DELTA)
                  (< (* 2 in-flow-window) target-in-flow-window))
          (queue-frame (frame type:WINDOW_UPDATE 0 streamid (fp:window_update delta))))))

    ;; ------------------------------------------------------------
    ;; Errors

    (field [info-for-exn
            (hasheq 'version 'http/2
                    'request req
                    'streamid streamid
                    'received 'unknown)])

    (define/public (get-info-for-exn) info-for-exn)

    (define/public (set-received! received)
      (unless (eq? (hash-ref info-for-exn 'received) received)
        (set! info-for-exn (hash-set info-for-exn 'received received))))

    (define/public (connection-error errorcode [debug #""])
      (send conn connection-error errorcode debug))

    (define/public (stream-error errorcode [msg #f] [wrapped-exn #f])
      (log-http2-debug "~a stream error: code = ~s, message = ~e" ID errorcode msg)
      (queue-frame (frame type:RST_STREAM 0 streamid (fp:rst_stream errorcode)))
      (signal-ua-error #f errorcode msg wrapped-exn)
      (raise 'stream-error))

    (define/private (signal-ua-error conn-error? errorcode msg wrapped-exn)
      (send-exn-to-user
       (build-exn (cond [msg msg]
                        [conn-error? "connection error signaled by user agent"]
                        [else "stream error signaled by user agent"])
                  (hash-set* (if wrapped-exn
                                 (hash-set info-for-exn 'wrapped-exn wrapped-exn)
                                 info-for-exn)
                             'code (if conn-error? 'ua-connection-error 'ua-stream-error)
                             'http2-error (decode-error-code errorcode)
                             'http2-errorcode errorcode)))
      (change-pstate! (make-done-pstate)))

    ;; ------------------------------------------------------------
    ;; User communication

    ;; Stage 1. User sends request data to this object.
    (define-values (in-from-user user-out) (make-pipe))
    ;; Stage 2. This object sends response object (or exn) to user.
    (define resp-bxe (make-box-evt))
    ;; Stage 3. This object sends response body to user.
    ;; In-flow control tied to user's consumption of data. That is, when user
    ;; consumes N bytes, request another N bytes from server.
    (define-values (user-in out-to-user raise-user-in-exn) (make-wrapped-pipe))
    ;; Stage 4. This object sends trailer to user.
    (define trailerbxe (make-box-evt))

    (define/public (send-exn-to-user e)
      (box-evt-set! resp-bxe (lambda () (raise e)))
      (box-evt-set! trailerbxe (lambda () (raise e)))
      (unless (port-closed? out-to-user)
        (raise-user-in-exn e))
      (log-http2-debug "~a sent exn to user: ~e" ID e))

    (define/public (get-user-communication)
      (values (make-pump-data-out) resp-bxe))

    (define/private ((make-pump-data-out))
      ;; run in user thread:
      (define data (request-data req))
      (cond [(not data)
             (close-output-port user-out)]
            [(bytes? data)
             (write-bytes data user-out)
             (close-output-port user-out)]
            [else
             (define user-out* (proxy-output-port user-out))
             (with-handler (lambda (e)
                             ;; Leave user-out open, so we don't treat the
                             ;; request as complete.
                             (send conn register-user-abort-request this e))
               (dynamic-wind
                 void
                 (lambda ()
                   (call-with-continuation-barrier
                    (lambda ()
                      (data user-out*)
                      ;; On normal return, close real user-out to signal complete.
                      (close-output-port user-out))))
                 (lambda ()
                   ;; Close proxy, so user can't cause mischief after return/escape.
                   (close-output-port user-out*))))]))

    ;; ------------------------------------------------------------
    ;; State machine

    ;; A StreamState is one of:
    ;; - 'idle                  S C
    ;; - 'reserved/local        S
    ;; - 'reserved/remote         C
    ;; - 'open                  S C
    ;; - 'half-closed/remote    S C
    ;; - 'half-closed/local     S C
    ;; - 'closed/by-me-recently S C -- server might not know closed yet
    ;; - 'closed                S C -- server knows closed
    (field [state 'idle])

    (define/private (check-state transition)
      ;; Section 5.1; (receive) transition is either a frame type (except
      ;; 'priority, allowed anywhere) or 'end_stream.
      (define (bad)
        (define fmt "bad state transition\n  transition: ~e\n  state: ~e")
        (connection-error error:PROTOCOL_ERROR (format fmt transition state)))
      (case state
        [(idle)
         (case transition
           [(headers) (set-state! 'open)]
           [(rst_stream) (set-state! 'closed)]
           [else (bad)])]
        [(reserved/local) ;; Only for servers
         (case transition
           [(rst_stream) (set-state! 'closed)]
           [(window_update) (void)]
           [else (bad)])]
        [(reserved/remote)
         (case transition
           [(headers) (set-state! 'half-closed/local)]
           [(rst_stream) (set-state! 'closed)]
           [else (bad)])]
        [(open)
         (case transition
           [(end_stream) (set-state! 'half-closed/remote)]
           [(rst_stream) (set-state! 'closed)]
           ;; RFC says "any type"
           [(data headers push_promise window_update) (void)]
           [else (bad)])]
        [(half-closed/local)
         (case transition
           [(end_stream) (set-state! 'closed)]
           [(rst_stream) (set-state! 'closed)]
           ;; RFC says "any type"
           [(data headers push_promise window_update) (void)]
           [else (bad)])]
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
        (stream-error error:INTERNAL_ERROR msg))
      (case state
        [(idle)
         (case transition
           [(headers) (set-state! 'open)]
           [(rst_stream) (set-state! 'closed/by-me-recently)]
           [else (my-error)])]
        [(reserved/local) ;; Only for servers
         (case transition
           [(headers) (set-state! 'half-closed/remote)]
           [(rst_stream) (set-state! 'closed/by-me-recently)]
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
           [else (my-error)])]
        [(half-closed/local)
         (case transition
           [(window_update) (void)]
           [(rst_stream) (set-state! 'closed/by-me-recently)]
           [else (my-error)])]
        [(half-closed/remote)
         (case transition
           [(rst_stream) (set-state! 'closed/by-me-recently)]
           ;; RFC says "any type"
           [(data headers push_promise window_update) (void)]
           [else (stream-error error:STREAM_CLOSED)])]
        [(closed closed/by-me-recently)
         (case transition
           [(rst_stream) (raise 'escape-without-error)]
           [else (my-error)])]))

    (define/private (set-state! new-state)
      #;(log-http2-debug "~a state ~s => ~s" ID state new-state)
      (define old-state state)
      (set! state new-state)
      (when (eq? new-state 'closed) ;; but not 'closed/by-me-recently
        (log-http2-debug "~a removing stream closed by server" ID)
        (send conn remove-stream streamid)))

    ;; ------------------------------------------------------------
    ;; Handling frames from server, other events

    (define/public (handle-data-payload flags payload)
      (match-define (fp:data padlen data) payload)
      (check-state 'data)
      (let ([len (payload-length flags payload)])
        (adjust-in-flow-window (- len)))
      (send pstate handle-data data)
      (when (flags-has? flags flag:END_STREAM)
        (check-state 'end_stream)
        (send pstate handle-end-stream)))

    (define/public (handle-headers-payload flags payload)
      (match-define (fp:headers padlen _streamdep _weight hs) payload)
      (check-state 'headers)
      (send pstate handle-headers hs)
      (define end? (flags-has? flags flag:END_STREAM))
      (when (flags-has? flags flag:END_STREAM)
        (check-state 'end_stream)
        (send pstate handle-end-stream)))

    (define/public (handle-priority-payload flags payload)
      (match-define (fp:priority streamdep weight) payload)
      ;; Allowed in any state
      (void))

    (define/public (handle-push_promise-payload flags payload)
      (match-define (fp:push_promise padlen promised-streamid header) payload)
      (check-state 'push_promise)
      (send pstate handle-push_promise promised-streamid header))

    (define/public (handle-rst_stream errorcode)
      (check-state 'rst_stream)
      (send pstate handle-rst_stream errorcode))

    (define/public (handle-goaway last-streamid errorcode debug)
      (check-state 'rst_stream) ;; pretend
      (send pstate handle-goaway last-streamid errorcode debug))

    (define/public (handle-window_update flags delta)
      (adjust-out-flow-window delta))

    (define/public (handle-user-abort e)
      ;; Note: server may have already processed request; see 8.1. So keep 'unknown.
      (stream-error error:CANCEL "request canceled by exception from data procedure" e))

    (define/public (handle-ua-connection-error errorcode comment)
      (define msg (format "user agent signaled connection error\n  reason: ~a" comment))
      (signal-ua-error #t errorcode msg #f))

    (define/public (handle-eof)
      (check-state 'rst_stream) ;; pretend
      (send pstate handle-eof))

    (define/public (handle-timeout)
      (check-send-state 'rst_stream) ;; pretend
      (send pstate handle-timeout))

    ;; ------------------------------------------------------------
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

    (define/public (remove-stream)
      (send conn remove-stream streamid))

    ;; ------------------------------------------------------------
    ;; Send request header

    ;; Must send the request immediately upon creating the stream, otherwise if
    ;; we create two streams and send the later streamid's request first, it
    ;; implicitly closes the earlier stream! (See HTTP/2 spec.)

    ;; Another alternative would be to delay allocating the streamid until
    ;; sending the request.

    (define/private (send-request req)
      (match-define (request method url header data) req)
      (log-http2-debug "~a initiating ~s request" ID method)
      (define pseudo-header (make-pseudo-header method url))
      (define enc-header (encode-header (append pseudo-header header)
                                        (send conn get-sending-dt)))
      (define no-content?
        (or (eq? data #f)
            (and (bytes? data) (zero? (bytes-length data)))))
      (queue-frames (make-header-frames enc-header no-content?))
      (cond [no-content? (change-pstate! (make-expect-header-pstate))]
            [else (change-pstate! (make-sending-request-data-pstate))]))

    (define/private (make-header-frames enc-header no-content?)
      (define len (bytes-length enc-header))
      (define frame-size (send conn get-config-value 'max-frame-size))
      ;; Note: if padding or priority flags set, must adjust frame-size!
      (for/list ([start (in-range 0 len frame-size)] [index (in-naturals)])
        (define end (min (+ start frame-size) len))
        (define flags
          (+ (if (and (zero? index) no-content?) flag:END_STREAM 0)
             (if (= end len) flag:END_HEADERS 0)))
        (define headerbf (subbytes enc-header start end))
        (if (zero? index)
            (frame type:HEADERS flags streamid (fp:headers 0 0 0 headerbf))
            (frame type:CONTINUATION flags streamid (fp:continuation headerbf)))))

    ;; FIXME: 8.1.2.2 forbids Connection header, others
    (define/private (make-pseudo-header method u)
      (list (list #":method" (symbol->bytes method))
            (list #":authority" (url-authority->bytes u)) ;; SHOULD use instead of Host
            (list #":scheme" (string->bytes/utf-8 (url-scheme u)))
            (list #":path" (let ([path (url-path/no-fragment->bytes u)])
                             (if (equal? path #"") #"/" path)))))

    (define/private (url-path/no-fragment->bytes u)
      (string->bytes/utf-8
       (url->string (url #f #f #f #f (url-path-absolute? u) (url-path u) (url-query u) #f))))
    (define/private (url-authority->bytes u)
      (string->bytes/utf-8
       (if (url-port u) (format "~a:~a" (url-host u) (url-port u)) (url-host u))))

    ;; ------------------------------------------------------------
    ;; Protocol state

    (field [pstate (new idle-pstate% (stream this))])

    ;; The manager thread syncs on this event to check if async work is
    ;; available to be done on this stream (and to do it).
    (define/public (get-work-evt)
      ;; Use guard-evt so manager automatically gets state changes without
      ;; having to rescan all streams.
      (guard-evt (lambda () (send pstate get-work-evt))))

    (define/public (change-pstate! new-pstate)
      (set! pstate new-pstate))

    (define/private (make-sending-request-data-pstate)
      (new sending-request-data-pstate% (stream this)
           (in-from-user in-from-user)))
    (define/public (make-expect-header-pstate)
      (new expect-header-pstate% (stream this)
           (req req) (resp-bxe resp-bxe) (user-in user-in) (trailerbxe trailerbxe)))
    (define/public (make-reading-response-pstate)
      (new reading-response-pstate% (stream this)
           (user-in user-in) (out-to-user out-to-user) (trailerbxe trailerbxe)))
    (define/public (make-done-pstate)
      (new done-pstate% (stream this)))

    ;; ------------------------------------------------------------
    ;; Finish initialization

    (cond [send-req? (send-request req)]
          [else (change-pstate! (make-expect-header-pstate))])
    ))

(define (make-header-promise stream header-entries label)
  (delay/sync
   (with-handler (lambda (e)
                   (h2-error (format "error processing ~a" label)
                             #:base-info (send stream get-info-for-exn)
                             #:info (hasheq 'wrapped-exn e)))
     (make-header-from-entries header-entries))))


;; ============================================================

;; A ProtocolState (pstate) extends the following base class:
(define pstate-base%
  (class object%
    (init-field stream)
    (super-new)

    (field [work-evt never-evt])
    (define/public (get-work-evt) work-evt)
    (define/public (set-work-evt! evt) (set! work-evt evt))

    (define/public (get-conn) (send stream get-conn))
    (define/public (get-info-for-exn) (send stream get-info-for-exn))
    (define/public (queue-frame fr) (send stream queue-frame fr))
    (define/public (queue-frames frs) (send stream queue-frames frs))

    (define/public (change-pstate! pstate)
      (begin (teardown) (send stream change-pstate! pstate)))
    (define/public (send-exn-to-user e) (send stream send-exn-to-user e))

    ;; Hooks for overriding:

    (abstract about)
    (define/public (teardown) (void))

    (define/public (handle-data data)
      (bad-tx 'DATA))
    (define/public (handle-headers header-entries)
      (bad-tx 'HEADERS))
    (define/public (handle-push_promise promised-streamid header-entries)
      (bad-tx 'PUSH_PROMISE))
    (define/public (handle-end-stream)
      (send stream stream-error error:PROTOCOL_ERROR
            (format "unexpected END_STREAM flag from server\n  protocol state: ~a" (about))))

    (define/private (bad-tx tx)
      (send stream stream-error error:PROTOCOL_ERROR
            (format "unexpected ~a frame from server\n  protocol state: ~a" tx (about))))

    ;; Default implementations, usually not overridden:

    (define/public (handle-rst_stream errorcode)
      (send-exn-to-user
       (build-exn "stream closed by server (RST_STREAM)"
                  (hash-set* (get-info-for-exn)
                             'code 'server-reset-stream
                             'http2-error (decode-error-code errorcode)
                             'http2-errorcode errorcode)))
      (change-pstate! (send stream make-done-pstate)))

    (define/public (handle-goaway last-streamid errorcode debug)
      ;; If this streamid > last-streamid, then server has not processed
      ;; this request, and it's okay to auto-retry on new connection.
      (when (> (send stream get-streamid) last-streamid)
        (send stream set-received! 'no)
        (send-exn-to-user
         (build-exn "connection closed by server (GOAWAY)"
                    (hash-set* (get-info-for-exn)
                               'code 'server-closed
                               'http2-error (decode-error-code errorcode)
                               'http2-errorcode errorcode)))
        (change-pstate! (send stream make-done-pstate))))

    (define/public (handle-eof)
      (send-exn-to-user
       (build-exn "connection closed by server (EOF)"
                  (hash-set* (get-info-for-exn)
                             'code 'server-EOF)))
      (change-pstate! (send stream make-done-pstate)))

    (define/public (handle-timeout)
      (send-exn-to-user
       (build-exn "connection closed by user agent (timeout)"
                  (hash-set* (get-info-for-exn)
                             'code 'ua-timeout)))
      (change-pstate! (send stream make-done-pstate)))
    ))

;; ------------------------------------------------------------

(define idle-pstate%
  (class pstate-base%
    (super-new)
    (define/override (about) 'idle)
    ))

;; ------------------------------------------------------------

(define sending-request-data-pstate%
  (class pstate-base%
    (init-field in-from-user)
    (inherit-field stream)
    (inherit set-work-evt! queue-frame change-pstate!)
    (super-new)

    (set-work-evt!
     (handle-evt (guard-evt
                  (lambda ()
                    (cond [(port-closed? in-from-user)
                           ;; We read an EOF (user-out is closed), so
                           ;; in-from-user is always ready now; ignore.
                           never-evt]
                          [(not (positive? (send stream get-effective-out-flow-window)))
                           ;; Only check if out-flow window is nonempty.
                           never-evt]
                          [else in-from-user])))
                 (lambda (ignored) (send-request-data-from-user))))

    (define/override (about) 'sending-request-data)

    (define/override (teardown)
      (close-input-port in-from-user))

    (define/override (handle-rst_stream errorcode)
      (cond [(= errorcode error:NO_ERROR)
             ;; Server already send response independent of data; see 8.1.
             (change-pstate! (send stream make-expect-header-pstate))]
            [else (super handle-rst_stream)]))

    (define/private (send-request-data-from-user)
      ;; PRE: in-from-user is ready for input
      (define streamid (send stream get-streamid))
      (define (check-at-eof?) ;; check for EOF w/o blocking
        (eof-object? (peek-bytes-avail!* (make-bytes 1) 0 #f in-from-user)))
      (define (send-data data end?)
        (queue-frame (frame type:DATA (if end? flag:END_STREAM 0) streamid
                            (fp:data 0 data)))
        ;; On EOF, close in-from-user so work-evt will ignore it.
        (when end? (close-input-port in-from-user)))
      (define allowed-len (send stream get-effective-out-flow-window))
      (define end?
        (let loop ([allowed-len allowed-len])
          (define len (min allowed-len (get-max-data-payload-length)))
          (define buf (make-bytes len))
          (define r (read-bytes-avail!* buf in-from-user))
          (define end? (or (eof-object? r) (check-at-eof?)))
          (cond [(eof-object? r) (begin (send-data #"" #t) #t)]
                [(zero? r) end?]
                [(< r len) (begin (send-data (subbytes buf 0 r) end?) end?)]
                [else (begin (send-data buf end?) (or end? (loop (- allowed-len r))))])))
      (when end?
        (change-pstate! (send stream make-expect-header-pstate))))

    (define/private (get-max-data-payload-length)
      (min (send (send stream get-conn) get-config-value 'max-frame-size)
           ;; FIXME: make my preference for max frame size configurable
           (expt 2 20)))
    ))

;; ------------------------------------------------------------

(define expect-header-pstate%
  (class pstate-base%
    (init-field req resp-bxe user-in trailerbxe)
    (inherit-field stream)
    (inherit change-pstate!)
    (super-new)

    (define/override (about) 'expect-header)

    (define/override (handle-headers header)
      (send stream set-received! 'yes)
      (cond [(informational-response? header)
             (log-http2-debug "~a discarding Informational header"
                              (send stream get-ID))
             ;; FIXME: do something with this header??
             (void)]
            [else
             (log-http2-debug "~a returning header to user" (send stream get-ID))
             (box-evt-set! resp-bxe (make-response-thunk header))
             (change-pstate! (send stream make-reading-response-pstate))]))

    (define/private (informational-response? header)
      (match header
        [(cons (list* #":status" (regexp #rx#"^1") _) _) #t]
        [_ #f]))

    (define/private (make-response-thunk header-entries0)
      ;; Create a promise so that work happens in user thread.
      (define-values (pseudos header-entries) (split-pseudo-header header-entries0))
      (define header-promise (make-header-promise stream header-entries "header"))
      (define resp-promise
        (delay/sync
         (define status
           (match (assoc #":status" pseudos)
             [(list #":status" (and status (regexp #rx#"^[1-5][0-9][0-9]$")))
              (string->number (bytes->string/latin-1 status))]
             [_
              (h2-error "bad or missing status from server"
                        #:base-info (send stream get-info-for-exn)
                        #:info (hasheq 'code 'bad-status))]))
         (define header (force header-promise))
         (new http2-response%
              (request req)
              (status-code status)
              (header header)
              (content user-in)
              (trailerbxe trailerbxe))))
      (lambda () (force resp-promise)))
    ))

(define (split-pseudo-header entries)
  (let loop ([acc null] [entries entries])
    (match entries
      [(cons (and pseudo (list (regexp #rx#"^:") _)) rest)
       (loop (cons pseudo acc) rest)]
      [_ (values acc entries)])))

;; ------------------------------------------------------------

(define reading-response-pstate%
  (class pstate-base%
    (init-field user-in out-to-user trailerbxe)
    (inherit-field stream)
    (inherit get-conn queue-frame set-work-evt! change-pstate!)
    (super-new)

    (define last-progress-evt (port-progress-evt user-in)) ;; progress or close!

    (set-work-evt!
     (handle-evt (guard-evt (lambda () last-progress-evt))
                 (lambda (ignored) (update-in-flow-window))))

    (define/override (about) 'reading-response)

    ;; FIXME: Check that we get DATA* HEADERS? END, not any (DATA | HEADERS)* END.

    (define/override (handle-data data)
      (write-bytes data out-to-user))

    (define/override (handle-end-stream)
      (close-output-port out-to-user)
      (change-pstate! (send stream make-done-pstate)))

    (define/override (handle-headers header-entries)
      (log-http2-debug "~a returning trailer to user" (send stream get-ID))
      (define trailer-promise (make-header-promise stream header-entries "trailer"))
      (box-evt-set! trailerbxe (lambda () (force trailer-promise))))

    (define/override (handle-push_promise promised-streamid header)
      (send (get-conn) handle-push_promise promised-streamid header))

    ;; Update flow window when user consumes data.
    (define/private (update-in-flow-window)
      (cond [(port-closed? user-in) ;; don't want more data
             (set! last-progress-evt never-evt)
             (define streamid (send stream get-streamid))
             (queue-frame (frame type:RST_STREAM 0 streamid (fp:rst_stream error:CANCEL)))
             (change-pstate! (send stream make-done-pstate))]
            [else ;; want increment more data
             (set! last-progress-evt (port-progress-evt user-in))
             ;; buffered : space used in pipe's buffer
             (define buffered (- (file-position out-to-user) (file-position user-in)))
             #;(log-http2-debug "user read from content buffer: buffered = ~s" buffered)
             (send stream update-in-flow-window buffered)]))
    ))

;; ------------------------------------------------------------

(define done-pstate%
  (class pstate-base%
    (inherit-field stream)
    (inherit set-work-evt!)
    (super-new)

    (set-work-evt!
     (handle-evt (alarm-evt (+ (current-inexact-milliseconds) KEEP-AFTER-CLOSE-MS))
                 (lambda (ignore)
                   (log-http2-debug "~a removing stream closed after delay"
                                    (send stream get-ID))
                   (send stream remove-stream))))

    (define/override (about) 'done)
    ))
