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
            [else
             (define stream (new http2-stream% (conn this) (streamid streamid)))
             (hash-set! stream-table streamid stream)
             (cond [(streamid-from-client? streamid)
                    (set! last-client-streamid streamid)]
                   [(streamid-from-server? streamid)
                    (set! last-server-streamid streamid)])]))

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
      (set! target-in-flow-window (+ target-in-flow-window increment)))

    (define/public (after-handle-frame)
      (when (< in-flow-window target-in-flow-window)
        (define diff (- target-in-flow-window in-flow-window))
        (queue-frame (frame type:UPDATE_WINDOW null 0 (fp:update_window diff)))
        (set! in-flow-window target-in-flow-window))
      (flush-frames))
    ))

;; ========================================

(define USER-PIPE-SIZE (expt 2 14))

(define http2-stream%
  (class* object% ()
    (init-field conn streamid
                [headers #f])
    (super-new)

    ;; Pipe to user: Goal is to tie flow control to user's consumption of
    ;; data. In particular, when user consumes N bytes, want to request another
    ;; N bytes from server. But how to measure (and sync on) user consumption?
    ;; Two ideas:
    ;;   1. make limited pipe, track writes; need data-queue and pump :(
    ;;      FIXME: allow user to configure pipe size
    ;;   2. use progress-evt on user-in and pipe-content-length ?
    ;;   3. wrap user-in to track reads (need to wrap for exns anyway...)
    (define-values (user-in out-to-user) (make-pipe USER-PIPE-SIZE))

    (define data-queue null)    ;; (Listof (cons Bytes Nat))
    (define close-to-user? #f)  ;; Boolean

    ;; Stream-local limit on flow-controlled {sends, receives}.
    (define out-flow-window INIT-FLOW-WINDOW)
    (define in-flow-window INIT-FLOW-WINDOW)

    (define/private (queue-frame fr) (send conn queue-frame))

    (define/public (on-stream-close)
      ;; FIXME: ...
      (send conn adjust-target-in-flow-window (- in-flow-window)))

    ;; ----------------------------------------

    (define/public (handle-data-payload flags payload)
      (match-define (fp:data padlen data) payload)
      ;; FIXME: check state machine
      (define end-stream? (flag-has? flags flag:END_STREAM))
      (let ([len (payload-length flags payload)])
        (adjust-in-flow-window (- len)))
      (set! data-queue (append data-queue (list (cons data 0))))
      (when end-stream? (set! close-to-user? #t))
      (define wrote (pump-to-user))
      (adjust-target-in-flow-window wrote)
      (void))

    (define/public (adjust-in-flow-window increment)
      (set! in-flow-window (+ in-flow-window increment))
      (send conn adjust-in-flow-window increment))

    (define/public (adjust-target-in-flow-window increment)
      (cond [(port-closed? user-in) ;; don't want more data
             (queue-frame (frame type:RST_STREAM null streamid error:CANCEL))]
            [else ;; want increment more data
             (set! in-flow-window (+ in-flow-window increment))
             (queue-frame (frame type:WINDOW_UPDATE null streamid (fp:window_update increment)))
             (send conn adjust-target-flow-window increment)]))

    (define/public (pump-to-user [acc 0]) ;; -> Nat, bytes written to user
      (match data-queue
        [(cons (cons data start) queue-rest)
         ;; If user-in is closed, then writes to out-to-user succeed w/o blocking.
         (define n (write-bytes-avail* data out-to-user start))
         (cond [(and n (> n 0))
                (define start* (+ start n))
                (cond [(= start* (bytes-length data))
                       (set! data-queue queue-rest)
                       (pump-to-user (+ n acc))]
                      [else
                       ;; FIXME: shrink data if less than half remaining?
                       (set! data-queue (cons (cons data start*) queue-rest))
                       (+ n acc)])]
               [else acc])]
        ['()
         (when close-to-user? (close-output-port out-to-user))
         0]))

    ;; ----------------------------------------

    (define/public (handle-headers-payload flags payload)
      (match-define (fp:headers padlen _streamdep _weight hs) payload)
      ;; FIXME: state machine
      (set! headers hs))

    (define/public (handle-priority-payload flags payload)
      (match-define (fp:priority streamdep weight) payload)
      ;; FIXME: state machine
      (void))

    (define/public (handle-push_promise-payload flags payload)
      (match-define (fp:push_promise padlen promised-streamid headers) payload)
      (send conn handle-push_promise promised-streamid headers))

    (define/public (handle-rst_stream-payload flags payload)
      (match-define (fp:rst_stream errorcode) payload)
      ;; FIXME: state machine
      ;; FIXME: propagate error to user-in
      (void))

    (define/public (handle-window-update flags increment)
      (set! out-flow-window (+ out-flow-window increment))
      (unless (< out-flow-window FLOW-CONTROL-BOUND)
        (stream-error error:FLOW_CONTROL_ERROR))
      ;; FIXME: Does this enable additional writes?
      (void))
    ))

;; ----------------------------------------

(define the-closed-stream
  '___)
