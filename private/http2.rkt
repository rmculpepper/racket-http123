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
         "h2-pack.rkt"
         (only-in "http11.rkt" make-decode-input-wrapper) ;; FIXME
         file/gunzip)
(provide (all-defined-out))

;; References:
;; - https://tools.ietf.org/html/rfc7540

(define http2-alpn-protocol #"h2")
(define http2-client-preface #"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n")

(define FLOW-WINDOW-BOUND (expt 2 31))
(define INIT-FLOW-WINDOW (sub1 (expt 2 16)))

(define KEEP-AFTER-CLOSE-MS (* 2 1000.0)) ;; keep closed stream for 2s

(define init-config
  (hasheq 'header-table-size 4096
          'enable-push 1
          'max-concurrent-streams +inf.0
          'initial-window-size (sub1 (expt 2 16))
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

    (define/public (get-sending-dt) sending-dt)

    ;; closed? : (U #f 'by-goaway 'by-error)
    ;; This state is used by the manager and by user threads.
    ;; The reader keeps reading until EOF.
    (define closed? #f)

    (define/private (set-closed! reason)
      (unless closed?
        (abandon-port out)
        (set! closed? reason)))

    ;; ----------------------------------------

    (define/public (queue-frame fr)
      (eprintf "--> ~a~e\n" (if closed? "DROPPING " "") fr)
      (unless closed? (write-frame out fr)))
    (define/public (queue-frames frs)
      (for ([fr (in-list frs)]) (queue-frame fr)))
    (define/public (flush-frames) (flush-output out))

    (define/public (connection-error errorcode [comment ""] #:debug [debug #""])
      (eprintf "!!! connection-error ~s, ~s\n" errorcode comment)
      (queue-frame (frame type:GOAWAY 0 0 (fp:goaway last-server-streamid errorcode debug)))
      (flush-frames)
      (set-closed! 'by-error)
      (error 'connection-error "bad")
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

    (define in-continue-frames null) ;; (Listof Frame), reversed

    (define/public (handle-frame fr)
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

    (define/public (handle-multipart-frames frs)
      (define rest-headerbfs
        (for/list ([fr (in-list (cdr frs))])
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
            (check-stream-nonzero)
            (send (stream) handle-rst_stream-payload flags payload)]
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
              (send stream handle-goaway-payload payload))
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

    (define/public (handle-connection-window_update flags increment)
      (set! out-flow-window (+ out-flow-window increment))
      (unless (< out-flow-window FLOW-WINDOW-BOUND)
        (connection-error error:FLOW_CONTROL_ERROR "window too large")))

    (define/public (adjust-in-flow-window increment)
      (set! in-flow-window (+ in-flow-window)))

    (define/public (adjust-target-flow-window increment)
      ;; FIXME: sending window_update frame is delayed until... ???
      (set! target-in-flow-window (+ target-in-flow-window increment)))

    (define/public (after-handle-frame)
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
                        (handle-frame fr)))))
      (define (streamsloop)
        (define stream-evts
          (for/list ([stream (in-hash-values stream-table)])
            (guard-evt (lambda () (send stream get-work-evt)))))
        (eprintf ".   manager streamsloop (~s)\n" (length stream-evts))
        (define streams-evt (apply choice-evt stream-evts))
        (let loop ()
          (eprintf ".   manager loop\n")
          (with-handlers ([(lambda (e) (eq? e 'escape-without-error)) void])
            (sync streams-evt
                  reader-evt
                  #;(wrap-evt (alarm-evt (+ (current-inexact-milliseconds) 10000.0))
                              (lambda (ignored) (eprintf "   manager is bored!\n")))))
          (flush-frames)
          (if (begin0 streams-changed? (set! streams-changed? #f))
              (streamsloop)
              (loop))))
      (with-handlers ([(lambda (e) (eq? e 'connection-error)) void])
        (streamsloop)))

    (define/private (reader)
      (cond [(eof-object? (peek-byte in))
             (eprintf "<-- EOF\n")
             (void)]
            [else
             (define fr (read-frame br))
             ;; FIXME: handle reading errors...
             (eprintf "<-- ~e\n" fr)
             (thread-send manager-thread fr void)
             (reader)]))

    (define manager-thread (thread (lambda () (manager))))
    (define reader-thread (thread (lambda () (reader))))

    ;; ========================================

    ;; called by user thread
    (define/public (open-request req)
      (define stream (new-client-stream req #t))
      ;; Stream automatically sends request headers.
      (define-values (user-out resp-headers-bxe user-in)
        (send stream get-user-communication))
      ;; User thread writes request content.
      ;; FIXME: optimize, send short data bytes on stream initialization
      (match (request-data req)
        [(? bytes? data)
         (write-bytes data user-out)
         (close-output-port user-out)]
        [#f (close-output-port user-out)]
        [(? procedure? put-data)
         (put-data (lambda (data) (write-bytes data user-out)))
         (close-output-port user-out)])
      ;; Get response header. (Note: may receive raised-exception instead!)
      (define resp-headers (sync resp-headers-bxe))
      ;; ----
      (define code (cond [(assoc #":status" resp-headers) => cdr] [else #f]))
      (define decode-mode
        (cond [(member (list #"content-encoding" #"gzip") resp-headers) 'gzip]
              [(member (list #"content-encoding" #"deflate") resp-headers) 'deflate]
              [else #f]))
      (define decoded-in (make-decode-input-wrapper decode-mode user-in))
      ;; FIXME
      (list (make-headers-from-lists resp-headers) decoded-in))

    ;; ========================================

    (define/private (send-handshake)
      (write-bytes http2-client-preface out)
      (define my-new-config my-config) ;; FIXME?
      (queue-frame (frame type:SETTINGS 0 0 (fp:settings null)))
      (set! my-configs/awaiting-ack (list my-new-config))
      (flush-frames))

    (send-handshake)

    ))

;; ========================================

(define http2-stream%
  (class* object% ()
    (init-field conn streamid
                req send-req?)
    (super-new)

    (define/public (client-originated?) (odd? streamid))
    (define/public (server-originated?) (even? streamid))
    (define/public (originator) (if (client-originated?) 'client 'server))

    ;; ============================================================
    ;; Stream Layer 1

    ;; This layer cares about the state machine described in Section 5. It does
    ;; not deal with the structure of HTTP requests.
    ;; - flow windows

    (define/private (connection-error errorcode [debug ""])
      (send conn connection-error errorcode debug))

    (define/private (stream-error errorcode)
      (queue-frame (frame type:RST_STREAM 0 streamid (fp:rst_stream errorcode)))
      (set-s2-state! 'done))

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
         (raise 'escape-without-error)]
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

    (define/private (state:closed? state)
      (case state
        [(closed/by-peer-rst closed/by-peer-end closed/by-me-recently closed/by-me-old) #t]
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

    (define/public (handle-rst_stream-payload flags payload)
      (match-define (fp:rst_stream errorcode) payload)
      (check-state 'rst_stream)
      (s2:handle-rst_stream errorcode))

    (define/public (handle-goaway-payload flags payload)
      (match-define (fp:goaway last-streamid errorcode debug) payload)
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
      (eprintf "#   new state = ~s\n" new-s2-state)
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

    (define/public (setup:done)
      (eprintf "... setting alarm\n")
      (set! s2-work-evt
            (handle-evt (alarm-evt (+ (current-inexact-milliseconds) KEEP-AFTER-CLOSE-MS))
                        (lambda (ignore)
                          (eprintf "... removing closed stream (~s)\n" streamid)
                          (send conn remove-stream streamid)))))

    ;; ----------------------------------------
    ;; Initiate request

    (define/private (initiate-request)
      (cond [send-req?
             (match-define (request method url headers data) req)
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

    (define/public (get-work-evt) s2-work-evt)

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

    ;; ----------------------------------------
    ;; Handling frames from server

    (define/public (s2:handle-data data end?)
      (write-bytes data out-to-user)
      (when end? (close-output-port out-to-user))
      (check-s2-state (if end? 'data+end 'data)))

    (define/public (s2:handle-headers headers end?)
      (eprintf ".   putting headers in box\n")
      (box-evt-set! resp-header-bxe (lambda () headers))
      (check-s2-state (if end? 'headers+end 'headers)))

    (define/public (s2:handle-push_promise promised-streamid headers)
      (send conn handle-push_promise promised-streamid headers))

    (define/public (s2:handle-rst_stream errorcode)
      (set-s2-state! 'done))

    (define/public (s2:handle-goaway last-streamid errorcode debug)
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
    (initiate-request)
    ))
