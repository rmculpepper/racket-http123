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

;; This class cares about frames, the state machine described in Section 5, and
;; flow control. The application state of HTTP requests is handled by `app`.
(define http2-stream%
  (class* object% ()
    (init-field conn streamid app)
    (super-new)

    (field [ID (format "~a.~a" (send conn get-ID) streamid)])

    ;; The manager thread syncs on this event to check if async work is
    ;; available to be done on this stream (and to do it).
    (define/public (get-work-evt)
      ;; Use guard-evt so manager automatically gets state changes without
      ;; having to rescan all streams.
      (guard-evt (lambda () (send app get-work-evt))))

    (define/public (get-config-value key)
      (send conn get-config-value key))

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

    (define/public (connection-error errorcode [debug #""])
      (send conn connection-error errorcode debug))

    (define/public (stream-error errorcode [msg #f] [wrapped-exn #f])
      (log-http2-debug "~a stream error: code = ~s, message = ~e" ID errorcode msg)
      (queue-frame (frame type:RST_STREAM 0 streamid (fp:rst_stream errorcode)))
      (send app handle-ua-error #f errorcode msg wrapped-exn)
      (raise 'stream-error))

    ;; ------------------------------------------------------------
    ;; State machine

    ;; A StreamState is one of:
    ;; - 'idle                  S C
    ;; - 'reserved/local        S
    ;; - 'reserved/remote         C
    ;; - 'open                  S C
    ;; - 'half-closed/remote    S C
    ;; - 'half-closed/local     S C
    ;; - 'closed/by-me-recently S C -- peer might not know closed yet
    ;; - 'closed                S C -- peer knows closed
    (field [state 'idle])

    (define/private (check-recv-state transition)
      ;; Section 5.1; (receive) transition is either a frame type (except
      ;; 'priority or 'window_update, allowed anywhere) or 'end_stream.
      (define (bad)
        (define fmt "bad state transition\n  transition: ~e\n  state: ~e")
        (connection-error error:PROTOCOL_ERROR (format fmt transition state)))
      (case state
        [(idle)
         (case transition
           [(headers) (set-state! 'open)]
           [(rst_stream) (set-state! 'closed)]
           [(push_promise) (set-state! 'reserved/remote)]
           [else (bad)])]
        [(reserved/local) ;; Only for servers
         (case transition
           [(rst_stream) (set-state! 'closed)]
           [else (bad)])]
        [(reserved/remote) ;; Only for clients
         (case transition
           [(headers) (set-state! 'half-closed/local)]
           [(rst_stream) (set-state! 'closed)]
           [else (bad)])]
        [(open)
         (case transition
           [(end_stream) (set-state! 'half-closed/remote)]
           [(rst_stream) (set-state! 'closed)]
           [else #;(data headers push_promise) (void)])]
        [(half-closed/remote)
         (case transition
           [(rst_stream) (set-state! 'closed)]
           [else (stream-error error:STREAM_CLOSED)])]
        [(half-closed/local)
         (case transition
           [(end_stream) (set-state! 'closed)]
           [(rst_stream) (set-state! 'closed)]
           [else #;(data headers push_promise) (void)])]
        [(closed/by-me-recently)
         ;; Must ignore frames received in this state.
         (raise 'escape-without-error)]))

    (define/private (check-send-state transition)
      ;; Section 5.1
      (define (bad)
        (define fmt "internal error: bad send state transition\n  state: ~e\n  transition: ~e")
        (define msg (format fmt state transition))
        (stream-error error:INTERNAL_ERROR msg))
      (case state
        [(idle)
         (case transition
           [(headers) (set-state! 'open)]
           [(push_promise) (set-state! 'reserved/local)]
           [(rst_stream) (set-state! 'closed/by-me-recently)]
           [else (bad)])]
        [(reserved/local) ;; Only for servers
         (case transition
           [(headers) (set-state! 'half-closed/remote)]
           [(rst_stream) (set-state! 'closed/by-me-recently)]
           [else (bad)])]
        [(reserved/remote) ;; Only for clients
         (case transition
           [(rst_stream) (set-state! 'closed/by-me-recently)]
           [else (bad)])]
        [(open)
         (case transition
           [(end_stream) (set-state! 'half-closed/local)]
           [(rst_stream) (set-state! 'closed/by-me-recently)]
           [else #;(data headers push_promise) (void)])]
        [(half-closed/local)
         (case transition
           [(rst_stream) (set-state! 'closed/by-me-recently)]
           [else (bad)])]
        [(half-closed/remote)
         (case transition
           [(rst_stream) (set-state! 'closed/by-me-recently)]
           [else #;(data headers push_promise) (void)])]
        [(closed closed/by-me-recently)
         (case transition
           [(rst_stream) (raise 'escape-without-error)]
           [else (bad)])]))

    (define/private (set-state! new-state)
      #;(log-http2-debug "~a state ~s => ~s" ID state new-state)
      (set! state new-state)
      (when (eq? new-state 'closed) ;; but not 'closed/by-me-recently
        (log-http2-debug "~a removing stream closed by server" ID)
        (send conn remove-stream streamid)))

    ;; ------------------------------------------------------------
    ;; Handling frames from server, other events

    (define/public (set-received! received)
      (send app set-received! received))

    (define/public (handle-data-payload flags payload)
      (match-define (fp:data padlen data) payload)
      (check-recv-state 'data)
      (let ([len (payload-length flags payload)])
        (adjust-in-flow-window (- len)))
      (send app handle-data data)
      (when (flags-has? flags flag:END_STREAM)
        (check-recv-state 'end_stream)
        (send app handle-end-stream)))

    (define/public (handle-headers-payload flags payload)
      (match-define (fp:headers padlen _streamdep _weight hs) payload)
      (check-recv-state 'headers)
      (send app handle-headers hs)
      (define end? (flags-has? flags flag:END_STREAM))
      (when (flags-has? flags flag:END_STREAM)
        (check-recv-state 'end_stream)
        (send app handle-end-stream)))

    (define/public (handle-priority-payload flags payload)
      (match-define (fp:priority streamdep weight) payload)
      ;; Allowed in any state
      (void))

    (define/public (handle-push_promise-payload flags payload)
      (match-define (fp:push_promise padlen promised-streamid header) payload)
      (check-recv-state 'push_promise)
      (send app handle-push_promise promised-streamid header))

    (define/public (handle-rst_stream errorcode)
      (check-recv-state 'rst_stream)
      (send app handle-rst_stream errorcode))

    (define/public (handle-goaway last-streamid errorcode debug)
      ;; PRE: streamid > last-streamid; that is, peer does not acknowledge this stream
      (check-recv-state 'rst_stream) ;; pretend
      (send app handle-goaway errorcode debug))

    (define/public (handle-window_update flags delta)
      (adjust-out-flow-window delta))

    (define/public (handle-user-abort e)
      ;; Note: server may have already processed request; see 8.1. So keep 'unknown.
      (stream-error error:CANCEL "request canceled by exception from data procedure" e))

    (define/public (handle-ua-connection-error errorcode comment)
      (define msg (format "user agent signaled connection error\n  reason: ~a" comment))
      (send app handle-ua-error #t errorcode msg #f))

    (define/public (handle-eof)
      (check-recv-state 'rst_stream) ;; pretend
      (send app handle-eof))

    (define/public (handle-timeout)
      (check-send-state 'rst_stream) ;; pretend ; FIXME: shouldn't this depend on handle-timeout???
      (send app handle-timeout))

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
         #;(check-send-state 'window_update)]
        [_ (void)])
      ;; FIXME: avoid sending multiple RST_STREAM frames, etc
      (send conn queue-frame fr))

    (define/public (queue-frames frs)
      (for ([fr (in-list frs)]) (queue-frame fr)))

    (define/public (remove-stream)
      (send conn remove-stream streamid))

    ;; ------------------------------------------------------------
    ;; Finish initialization

    (send app start this)))


;; ============================================================
;; Application-level interface

(define app<%>
  (interface ()
    start                   ;; Stream -> Void
    get-work-evt            ;; -> Evt
    handle-data             ;; Bytes -> Void
    handle-end-stream       ;; ->  Void
    handle-headers          ;; (Listof HeaderEntry) -> Void
    handle-push_promise     ;; StreamID (Listof HeaderEntry) -> Void
    handle-rst_stream       ;; ErrorCode -> Void
    handle-goaway           ;; ErrorCode Any -> Void
    handle-eof              ;; -> Void
    handle-timeout          ;; -> Void
    handle-ua-error         ;; Boolean ErrorCode String Exn/#f -> Void
    ))

(define app-base%
  (class* object% (app<%>)
    (super-new)

    (field [stream #f])
    (field [appstate #f])

    (define/public (start new-stream)
      (set! stream new-stream)
      (set! appstate (make-initial-state))
      (send appstate start))

    (abstract make-initial-state)

    (define/public (get-stream) stream)
    (define/public (set-stream! v) (set! stream v))
    (define/public (set-appstate! v) (set! appstate v))

    (define/public (get-work-evt)
      (send appstate get-work-evt))
    (define/public (handle-data data)
      (send appstate handle-data data))
    (define/public (handle-end-stream)
      (send appstate handle-end-stream))
    (define/public (handle-headers header-entries)
      (send appstate handle-headers header-entries))
    (define/public (handle-push_promise streamid header-entries)
      (send appstate handle-push_promise streamid header-entries))
    (define/public (handle-rst_stream errorcode)
      (send appstate handle-rst_stream errorcode))
    (define/public (handle-goaway errorcode debug)
      (send appstate handle-goaway errorcode debug))
    (define/public (handle-eof)
      (send appstate handle-eof))
    (define/public (handle-timeout)
      (send appstate handle-timeout))

    (abstract handle-ua-error)
    (abstract default-handle-rst_stream)
    (abstract default-handle-goaway)
    (abstract default-handle-eof)
    (abstract default-handle-timeout)

    (define/public (handle-unexpected thing kind)
      (send stream stream-error error:PROTOCOL_ERROR
            (format "received unexpected ~a ~a in state: ~a"
                    thing kind (object-name appstate))))
    ))

;; ------------------------------------------------------------

(define appstate-base%
  (class object%
    (init-field app)
    (super-new)

    (field [stream (get-field stream app)])

    (define/public (change-appstate! appstate)
      (begin (teardown) (send app set-appstate! appstate)))

    (define/public (handle-data data)
      (send app handle-unexpected 'DATA 'frame))
    (define/public (handle-end-stream)
      (send app handle-unexpected 'END_STEAM 'flag))
    (define/public (handle-headers header-entries)
      (send app handle-unexpected 'HEADERS 'frame))
    (define/public (handle-push_promise streamid header-entries)
      (send app handle-unexpected 'PUSH_PROMISE 'frame))

    (define/public (handle-rst_stream errorcode)
      (send app default-handle-rst_stream errorcode))
    (define/public (handle-goaway errorcode debug)
      (send app default-handle-goaway errorcode debug))
    (define/public (handle-eof)
      (send app default-handle-eof))
    (define/public (handle-timeout)
      (send app default-handle-timeout))

    ;; Hooks for overriding:
    (define/public (get-work-evt) never-evt)
    (define/public (teardown) (void))
    ))

(define send-data-state-base%
  (class appstate-base%
    (init-field in-from-user)
    (inherit-field app stream)
    (super-new)

    (define/override (get-work-evt)
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
                  (lambda (ignored) (send-data-from-user))))

    (define/override (teardown)
      (close-input-port in-from-user))

    (define/private (send-data-from-user)
      ;; PRE: in-from-user is ready for input
      (define streamid (send stream get-streamid))
      (define (check-at-eof?) ;; check for EOF w/o blocking
        (eof-object? (peek-bytes-avail!* (make-bytes 1) 0 #f in-from-user)))
      (define (send-data data end?)
        (send stream queue-frame
              (frame type:DATA (if end? flag:END_STREAM 0) streamid (fp:data 0 data)))
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
      (when end? (done-sending)))

    (abstract done-sending)

    (define/private (get-max-data-payload-length)
      (min (send stream get-config-value 'max-frame-size)
           ;; FIXME: make my preference for max frame size configurable
           (expt 2 20)))
    ))

(define read-data-state-base%
  (class appstate-base%
    (init-field user-in out-to-user)
    (inherit-field app stream)
    (super-new)

    (define last-progress-evt (port-progress-evt user-in)) ;; progress or close!

    (define/override (get-work-evt)
      (handle-evt (guard-evt (lambda () last-progress-evt))
                  (lambda (ignored) (update-in-flow-window))))

    (define/override (handle-data data)
      (write-bytes data out-to-user))

    (define/override (handle-end-stream)
      (close-output-port out-to-user)
      #| override with change-appstate! |#)

    ;; Update flow window when user consumes data.
    (define/private (update-in-flow-window)
      (cond [(port-closed? user-in) ;; don't want more data
             (set! last-progress-evt never-evt)
             (handle-closed-user-in)]
            [else ;; want increment more data
             (set! last-progress-evt (port-progress-evt user-in))
             ;; buffered : space used in pipe's buffer
             (define buffered (- (file-position out-to-user) (file-position user-in)))
             #;(log-http2-debug "user read from content buffer: buffered = ~s" buffered)
             (send stream update-in-flow-window buffered)]))

    (abstract handle-closed-user-in)
    ))

(define done-state%
  (class appstate-base%
    (init-field [closed-ms (current-inexact-milliseconds)])
    (inherit-field app stream)
    (super-new)

    (define/override (get-work-evt)
      (handle-evt (alarm-evt (+ closed-ms KEEP-AFTER-CLOSE-MS))
                  (lambda (ignore)
                    (log-http2-debug "~a removing stream closed after delay"
                                     (send stream get-ID))
                    (send stream remove-stream))))
    ))
