;; Copyright 2021 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/class
         racket/match
         racket/promise
         net/url-string
         scramble/evt
         "interfaces.rkt"
         "header.rkt"
         "response.rkt"
         (submod "util.rkt" port)
         "request.rkt"
         "h2-frame.rkt"
         "hpack.rkt"
         "h2-stream.rkt")
(provide (all-defined-out))

;; The classes in this module implement the HTTP application-level protocol.

;; Shared state for a single client stream. Also acts as factory for
;; application state objects.
(define h2-client-stream-app%
  (class* app-base% ()
    (init-field req)
    (inherit-field stream appstate)
    (super-new)

    (field [received 'unknown])
    (define/public (set-received! v) (set! received v))

    ;; ----------------------------------------
    ;; Errors

    (define/public (get-info-for-exn)
      (hasheq 'version 'http/2
              'request req
              'streamid (and stream (send stream get-streamid))
              'received received))

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
      (log-http2-debug "~a sent exn to user: ~e" (send stream get-ID) e))

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
                             (define conn (send stream get-conn))
                             (send conn register-user-abort-request stream e))
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
    ;; Handlers

    (define/public (build/send-exn-to-user/done msg kvs #:wrapped-exn [wrapped-exn #f])
      (let* ([info (get-info-for-exn)]
             [info (if wrapped-exn (hash-set info 'wrapped-exn wrapped-exn) info)]
             [info (apply hash-set* info kvs)])
        (send-exn-to-user (build-exn msg info))
        (send appstate change-appstate! (make-done-state))))

    (define/override (handle-ua-error conn-error? errorcode msg wrapped-exn)
      (build/send-exn-to-user/done
       (cond [msg msg]
             [conn-error? "connection error signaled by user agent"]
             [else "stream error signaled by user agent"])
       (list 'code (if conn-error? 'ua-connection-error 'ua-stream-error)
             'http2-error (decode-error-code errorcode)
             'http2-errorcode errorcode)
       #:wrapped-exn wrapped-exn))

    (define/override (default-handle-rst_stream errorcode)
      (when (equal? errorcode error:REFUSED_STREAM) ;; 8.1.4
        (set-received! 'no))
      (build/send-exn-to-user/done
       "stream closed by server (RST_STREAM)"
       (list 'code 'server-reset-stream
             'http2-error (decode-error-code errorcode)
             'http2-errorcode errorcode)))

    (define/override (default-handle-goaway errorcode debug)
      (set-received! 'no) ;; 8.1.4
      (build/send-exn-to-user/done
       "connection closed by server (GOAWAY)"
       (list 'code 'server-closed
             'http2-error (decode-error-code errorcode)
             'http2-errorcode errorcode)))

    (define/override (default-handle-eof)
      (build/send-exn-to-user/done
       "connection closed by server (EOF)"
       (list 'code 'server-EOF)))

    (define/override (default-handle-timeout)
      (build/send-exn-to-user/done
       "connection closed by user agent (timeout)"
       (list 'code 'ua-timeout)))

    ;; ------------------------------------------------------------
    ;; Protocol state

    (define/override (make-initial-state)
      (new send-request-state% (app this) (req req)))
    (define/public (make-sending-request-data-state)
      (new sending-request-data-state% (app this)
           (in-from-user in-from-user)))
    (define/public (make-expect-header-state)
      (new expect-header-state% (app this)
           (req req) (resp-bxe resp-bxe) (user-in user-in) (trailerbxe trailerbxe)))
    (define/public (make-reading-response-state)
      (new reading-response-state% (app this)
           (user-in user-in) (out-to-user out-to-user) (trailerbxe trailerbxe)))
    (define/public (make-done-state)
      (new done-state% (app this)))
    ))

(define (make-header-promise app header-entries label)
  (delay/sync
   (with-handler (lambda (e)
                   (h2-error (format "error processing ~a" label)
                             #:base-info (send app get-info-for-exn)
                             #:info (hasheq 'wrapped-exn e)))
     (make-header-from-h2-entries header-entries))))

;; ============================================================

(define send-request-state%
  (class* appstate-base% ()
    (init-field req)
    (inherit-field app stream)
    (inherit change-appstate!)
    (super-new)

    (define/public (start) (send-request req))

    ;; ------------------------------------------------------------
    ;; Send request header

    ;; Must send the request immediately upon creating the stream, otherwise if
    ;; we create two streams and send the later streamid's request first, it
    ;; implicitly closes the earlier stream! (See HTTP/2 spec.)

    ;; Another alternative would be to delay allocating the streamid until
    ;; sending the request.

    (define/private (send-request req)
      (match-define (request method url header data) req)
      (log-http2-debug "~a initiating ~s request" (send stream get-ID) method)
      (define pseudo-header (make-pseudo-header method url))
      (define enc-header (encode-header (append pseudo-header header)
                                        (let ([conn (send stream get-conn)])
                                          (send conn get-sending-dt))))
      (define no-content?
        (or (eq? data #f)
            (and (bytes? data) (zero? (bytes-length data)))))
      (send stream queue-frames (make-header-frames enc-header no-content?))
      (cond [no-content? (change-appstate! (send app make-expect-header-state))]
            [else (change-appstate! (send app make-sending-request-data-state))]))

    (define/private (make-header-frames enc-header no-content?)
      (define streamid (send stream get-streamid))
      (define frame-size (send stream get-config-value 'max-frame-size))
      (make-header-frames* enc-header no-content? streamid frame-size))

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
    ))

(define (make-header-frames* enc-header no-content? streamid frame-size)
  (define len (bytes-length enc-header))
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

;; ------------------------------------------------------------

(define sending-request-data-state%
  (class send-data-state-base%
    (inherit-field app)
    (inherit change-appstate!)
    (super-new)

    (define/override (done-sending)
      (change-appstate! (send app make-expect-header-state)))

    (define/override (handle-rst_stream errorcode)
      (cond [(= errorcode error:NO_ERROR)
             ;; Server already send response independent of data; see 8.1.
             (change-appstate! (send app make-expect-header-state))]
            [else (super handle-rst_stream)]))
    ))

;; ------------------------------------------------------------

(define expect-header-state%
  (class appstate-base%
    (init-field req resp-bxe user-in trailerbxe)
    (inherit-field app stream)
    (inherit change-appstate!)
    (super-new)

    (define/override (handle-headers header)
      (send app set-received! 'yes)
      (cond [(informational-response? header)
             (log-http2-debug "~a discarding Informational header" (send stream get-ID))
             ;; FIXME: do something with this header??
             (void)]
            [else
             (log-http2-debug "~a returning header to user" (send stream get-ID))
             (box-evt-set! resp-bxe (make-response-thunk header))
             (change-appstate! (send app make-reading-response-state))]))

    (define/private (informational-response? header)
      (match header
        [(cons (list* #":status" (regexp #rx#"^1") _) _) #t]
        [_ #f]))

    (define/private (make-response-thunk header-entries0)
      ;; Create a promise so that work happens in user thread.
      (define-values (pseudos header-entries) (split-pseudo-header header-entries0))
      (define header-promise (make-header-promise app header-entries "header"))
      (define resp-promise
        (delay/sync
         (define status
           (match (assoc #":status" pseudos)
             [(list* #":status" (and status (regexp #rx#"^[1-5][0-9][0-9]$")) _)
              (string->number (bytes->string/latin-1 status))]
             [_
              (h2-error "bad or missing status from server"
                        #:base-info (send app get-info-for-exn)
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
      [(cons (and pseudo (list* (regexp #rx#"^:") _)) rest)
       (loop (cons pseudo acc) rest)]
      [_ (values acc entries)])))

;; ------------------------------------------------------------

(define reading-response-state%
  (class read-data-state-base%
    (init-field trailerbxe)
    (inherit-field app stream)
    (inherit change-appstate!)
    (super-new)

    ;; FIXME: Check that we get DATA* HEADERS? END, not any (DATA | HEADERS)* END.

    (define/override (handle-closed-user-in)
      (define streamid (send stream get-streamid))
      (send stream queue-frame
            (frame type:RST_STREAM 0 streamid (fp:rst_stream error:CANCEL)))
      (change-appstate! (send app make-done-state)))

    (define/override (handle-end-stream)
      (super handle-end-stream)
      (box-evt-set! trailerbxe (lambda () #f)) ;; FIXME?
      (change-appstate! (send app make-done-state)))

    (define/override (handle-headers header-entries)
      (log-http2-debug "~a returning trailer to user" (send stream get-ID))
      (define trailer-promise (make-header-promise app header-entries "trailer"))
      (box-evt-set! trailerbxe (lambda () (force trailer-promise))))

    (define/override (handle-push_promise promised-streamid header)
      (error 'handle-push_promise "not implemented"))
    ))
