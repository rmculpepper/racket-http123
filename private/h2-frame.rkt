#lang racket/base
(require racket/match
         binaryio/integer
         binaryio/reader)
(provide (all-defined-out))

(define (streamid-from-server? streamid)
  (even? streamid))
(define (streamid-from-client? streamid)
  (odd? streamid))

;; ----------------------------------------

(define (write-frame out fr)
  (match-define (frame type flags streamid payload) fr)
  (write-frame-header out (payload-length flags payload) type flags streamid)
  (write-frame-payload out flags payload))

(define (write-frame-header out len type flags streamid)
  (write-integer len 3 #f out #t)
  (write-byte type out)
  (write-byte flags out)
  (write-integer streamid 4 #f out #t))

(define (write-frame-payload out flags payload)
  (define (write-padlen padlen)
    (when (flags-has? flags flag:PADDED) (write-byte padlen out)))
  (define (write-padding padlen)
    (when (flags-has? flags flag:PADDED) (write-bytes (make-bytes padlen 0) out)))
  (define (write-streamid streamid)
    (write-integer streamid 4 #f out))
  (match payload
    [(fp:data padlen data)
     (write-padlen padlen)
     (write-bytes data out)
     (write-padding padlen)]
    [(fp:headers padlen streamdep weight headerbf)
     (write-padlen padlen)
     (when (flags-has? flags flag:PRIORITY)
       (write-streamid streamdep)
       (write-byte weight out))
     (write-bytes headerbf out)
     (write-padding padlen)]
    [(fp:priority streamdep weight)
     (write-streamid streamdep)
     (write-byte weight out)]
    [(fp:rst_stream errorcode)
     (write-integer errorcode 4 #f out)]
    [(fp:settings settings)
     (for ([s (in-list settings)])
       (match-define (setting key val) s)
       (write-integer (encode-setting-key key) 2 #f out)
       (write-integer val 4 #f out))]
    [(fp:push_promise padlen promised-streamid headerbf)
     (write-padlen padlen)
     (write-streamid promised-streamid)
     (write-bytes headerbf out)
     (write-padding padlen)]
    [(fp:ping opaque)
     (write-bytes opaque out)]
    [(fp:goaway last-streamid errorcode debug)
     (write-streamid last-streamid)
     (write-integer errorcode 4 #f out)
     (write-bytes debug out)]
    [(fp:window_update increment)
     (write-integer increment 4 #f out)]
    [(fp:continuation headerbf)
     (write-bytes headerbf out)]
    [(? bytes? bs)
     (write-bytes bs out)]))

(define (payload-length flags payload)
  (define (padding padlen)
    (if (flags-has? flags flag:PADDED) (add1 padlen) 0))
  (match payload
    [(fp:data padlen data)
     (+ (padding padlen) (bytes-length data))]
    [(fp:headers padlen streamdep weight headerbf)
     (+ (padding padlen)
        (if (flags-has? flags flag:PRIORITY) (+ 4 1) 0)
        (bytes-length headerbf))]
    [(fp:priority streamdep weight)
     (+ 4 1)]
    [(fp:rst_stream errorcode)
     (+ 4)]
    [(fp:settings settings)
     (* 6 (length settings))]
    [(fp:push_promise padlen promised-streamid headerbf)
     (+ (padding padlen) 4 (bytes-length headerbf))]
    [(fp:ping opaque)
     (+ (bytes-length opaque))]
    [(fp:goaway last-streamid errorcode debug)
     (+ 4 4 (bytes-length debug))]
    [(fp:window_update increment)
     (+ 4)]
    [(fp:continuation headerbf)
     (+ (bytes-length headerbf))]
    [(? bytes? bs)
     (bytes-length bs)]))

(define (trim-streamid n)
  (bitwise-bit-field n 0 31))

;; ----------------------------------------

(define (read-frame br)
  (define-values (len type flags streamid) (read-frame-header br))
  (define payload
    (b-call/save-limit br (lambda ()
                            (b-push-limit br len)
                            (read-frame-payload br type flags))))
  (frame type flags streamid payload))

(define (read-frame-header br)
  (define len (b-read-be-uint br 3))
  (define type (b-read-byte br))
  (define flags (b-read-byte br))
  (define stream-id (trim-streamid (b-read-be-uint br 4)))
  (values len type flags stream-id))

(define (read-streamid br)
  (trim-streamid (b-read-be-uint br 4)))

;; PRE: br limit set to payload length
(define (read-frame-payload br type flags)
  (define (discarding-padding proc)
    (define padlen (if (flags-has? flags flag:PADDED) (b-read-byte br) 0))
    (begin0 (b-call/save-limit br (lambda ()
                                    (define len (- (b-get-limit br) padlen))
                                    (when (< len 0) '_) ;; FIXME
                                    (b-push-limit br len)
                                    (proc padlen)))
      ;; Note: MAY check padding is zero. We don't.
      (b-read-bytes br padlen)))
  (define (rest-of-payload)
    (b-read-bytes br (b-get-limit br)))
  (match type
    [(== type:DATA)
     (discarding-padding
      (lambda (padlen)
        (define data (rest-of-payload))
        (fp:data padlen data)))]
    [(== type:HEADERS)
     (discarding-padding
      (lambda (padlen)
        (define streamdep (if (flags-has? flags flag:PRIORITY) (read-streamid br) #f))
        (define weight (if (flags-has? flags flag:PRIORITY) (b-read-byte br) #f))
        (define headerbf (read-headerbf br))
        (fp:headers padlen streamdep weight headerbf)))]
    [(== type:PRIORITY)
     (define streamdep (read-streamid br))
     (define weight (b-read-byte br))
     (fp:priority streamdep weight)]
    [(== type:RST_STREAM)
     (define errorcode (b-read-be-uint br 4))
     (fp:rst_stream errorcode)]
    [(== type:SETTINGS)
     (define (read-setting)
       (define key (b-read-be-uint br 2))
       (define val (b-read-be-uint br 4))
       (setting (decode-setting-key key) val))
     (define settings
       (let loop () (if (b-at-limit? br) null (cons (read-setting) (loop)))))
     (fp:settings settings)]
    [(== type:PUSH_PROMISE)
     (discarding-padding
      (lambda (padlen)
        (define promised-streamid (read-streamid br))
        (define headerbf (read-headerbf br))
        (fp:push_promise padlen promised-streamid headerbf)))]
    [(== type:PING)
     (define opaque (b-read-bytes br 8))
     (fp:ping opaque)]
    [(== type:GOAWAY)
     (define last-streamid (read-streamid br))
     (define errorcode (b-read-be-uint br 4))
     (define debug (rest-of-payload))
     (fp:goaway last-streamid errorcode debug)]
    [(== type:WINDOW_UPDATE)
     (define increment (bitwise-bit-field (b-read-be-uint br 4) 0 30))
     (fp:window_update increment)]
    [(== type:CONTINUATION)
     (define headerbf (read-headerbf br))
     (fp:continuation headerbf)]
    [_ (rest-of-payload)]))

(define (read-headerbf br) ;; FIXME?
  (b-read-bytes br (b-get-limit br)))

;; ========================================

;; 4.2 Frame Size
;; max frame size is configurable, between 2^14 and 2^24-1
;; must send error code FRAME_SIZE_ERROR if gets too-big frame

;; 5.1.1 Stream Identifiers
;; - 0    -- connection control
;; - 1    -- upgrade from http/1.1
;; - odd  -- client-initiated
;; - even -- server-initiated
;; stream IDs must be monotonic increasing

;; 5.1.2 Stream Concurrency
;; SETTINGS_MAX_CONCURRENT_STREAMS

;; 5.2 Flow Control
;; initial flow-control window is 2^16-1

;; 5.3 Stream Priority
;; priority is just a suggestion

;; 5.4 Error Handling

;; on connection error, send GOAWAY frame and close connection
;; on stream error, send RST_STREAM
;; - cannot ignore additional messages for header compression state, flow control state
;; - must not send RST_STREAM in response to RST_STREAM

;; 5.5 Extending HTTP/2
;; implementations must ignore unknown extensions
;; implementations must discard frames with unknown/unsupported types
;; - except, unknown frame in middle of header block must be treated as connection error

;; ============================================================
;; 6 Frame Definitions

(struct frame (type flags streamid payload) #:transparent)

(define type:DATA #x00)
(define type:HEADERS #x01)
(define type:PRIORITY #x02)
(define type:RST_STREAM #x03)
(define type:SETTINGS #x04)
(define type:PUSH_PROMISE #x05)
(define type:PING #x06)
(define type:GOAWAY #x07)
(define type:WINDOW_UPDATE #x08)
(define type:CONTINUATION #x09)

(define (frame:data? f) (eqv? (frame-type f) type:DATA))
(define (frame:headers? f) (eqv? (frame-type f) type:HEADERS))
(define (frame:priority? f) (eqv? (frame-type f) type:PRIORITY))
(define (frame:rst_stream? f) (eqv? (frame-type f) type:RST_STREAM))
(define (frame:settings? f) (eqv? (frame-type f) type:SETTINGS))
(define (frame:push_promise? f) (eqv? (frame-type f) type:PUSH_PROMISE))
(define (frame:ping? f) (eqv? (frame-type f) type:PING))
(define (frame:goaway? f) (eqv? (frame-type f) type:GOAWAY))
(define (frame:window_update? f) (eqv? (frame-type f) type:WINDOW_UPDATE))

(define flag:ACK #x01)
(define flag:END_STREAM #x01)
(define flag:END_HEADERS #x04)
(define flag:PADDED #x08)
(define flag:PRIORITY #x20)

(define (frame-has-flag? f flag)
  (flags-has? (frame-flags f) flag))

(define (flags-has? flags flag)
  (= (bitwise-and flags flag) flag))

;; 6.1 DATA
;; Payload w/o padding: Data(*)
;; Flags: END_STREAM, PADDED
(struct fp:data (padlen data) #:prefab)

;; 6.2 HEADERS
;; Payload w/o padding: StreamDep(4,uint), { Weight(1) }?, HeaderBlockFragment(*)
;; Flags: END_STREAM, END_HEADERS, PADDED, PRIORITY
(struct fp:headers (padlen streamdep weight headerbf) #:prefab)

;; 6.3 PRIORITY
;; Payload: StreamDep(4,uint), Weight(1)
;; Flags: none
(struct fp:priority (streamdep weight) #:prefab)

;; 6.4 RST_STREAM
;; Payload: ErrorCode(4,uint)
;; Flags: none
(struct fp:rst_stream (errorcode) #:prefab)

;; 6.5 SETTINGS
;; Payload: { SettingId(2,uint), Value(4) }*
;; Flags: ACK
(struct fp:settings (settings) #:prefab)
(struct setting (key value) #:prefab)

(define (encode-setting-key key)
  (case key
    [(header-table-size) #x01]
    [(enable-push) #x02]
    [(max-concurrent-streams) #x03]
    [(initial-window-size) #x04]
    [(max-frame-size) #x05]
    [(max-header-list-size) #x06]
    [else key]))
(define (decode-setting-key n)
  (case n
    [(#x01) 'header-table-size]
    [(#x02) 'enable-push]
    [(#x03) 'max-concurrent-streams]
    [(#x04) 'initial-window-size]
    [(#x05) 'max-frame-size]
    [(#x06) 'max-header-list-size]
    [else n]))

;; 6.6 PUSH_PROMISE
;; Payload w/o padding: PromisedStreamID(4,uint), HeaderBlockFragment(*)
;; Flags: END_HEADERS, PADDED
(struct fp:push_promise (padlen promised-streamid headerbf) #:prefab)

;; 6.7 PING
;; Payload: Opaque(8)
;; Flags: ACK
(struct fp:ping (opaque) #:prefab)

;; 6.8 GOAWAY
;; Payload: LastStreamID(4,uint), ErrorCode(4,uint), AdditionalDebugData(*)
;; Flags: none
(struct fp:goaway (last-streamid errorcode debug) #:prefab)

;; 6.9 WINDOW_UPDATE
;; Payload: WindowSizeIncrement(4,uint)
;; Flags: none
(struct fp:window_update (increment) #:prefab)

;; 6.10 CONTINUATION
;; Payload: HeaderBlockFragment(*)
;; Flags: END_HEADERS
(struct fp:continuation (headerbf) #:prefab)

;; 7 Error Codes

(define error:NO_ERROR #x00)
(define error:PROTOCOL_ERROR #x01)
(define error:INTERNAL_ERROR #x02)
(define error:FLOW_CONTROL_ERROR #x03)
(define error:SETTINGS_TIMEOUT #x04)
(define error:STREAM_CLOSED #x05)
(define error:FRAME_SIZE_ERROR #x06)
(define error:REFUSED_STREAM #x07)
(define error:CANCEL #x08)
(define error:COMPRESSION_ERROR #x09)
(define error:CONNECT_ERROR #x0A)
(define error:ENHANCE_YOUR_CALM #x0B)
(define error:INADEQUATE_SECURITY #x0C)
(define error:HTTP_1_1_REQUIRED #x0D)

(define (decode-error-code code)
  (case code
    [(#x00) 'NO_ERROR]
    [(#x01) 'PROTOCOL_ERROR]
    [(#x02) 'INTERNAL_ERROR]
    [(#x03) 'FLOW_CONTROL_ERROR]
    [(#x04) 'SETTINGS_TIMEOUT]
    [(#x05) 'STREAM_CLOSED]
    [(#x06) 'FRAME_SIZE_ERROR]
    [(#x07) 'REFUSED_STREAM]
    [(#x08) 'CANCEL]
    [(#x09) 'COMPRESSION_ERROR]
    [(#x0A) 'CONNECT_ERROR]
    [(#x0B) 'ENHANCE_YOUR_CALM]
    [(#x0C) 'INADEQUATE_SECURITY]
    [(#x0D) 'HTTP_1_1_REQUIRED]
    [else #f]))
