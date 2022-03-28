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
    (inherit-field stream
                   recv-state
                   resp-bxe
                   trailerbxe
                   in-from-user
                   user-out
                   user-in
                   out-to-user
                   raise-user-in-exn)
    (super-new)

    (field [received 'unknown])
    (define/public (set-received! v) (set! received v))

    ;; ----------------------------------------
    ;; Errors

    (define/override (get-info-for-exn)
      (hash-set* (super get-info-for-exn) 'request req 'received received))

    (define/override (by-peer) "by server")

    (define/override (peer- sym)
      (case sym
        [(EOF) 'server-EOF]
        [(closed) 'server-closed]
        [(reset-stream) 'server-reset-stream]
        [else (string->symbol (format "server-~a" sym))]))

    ;; ----------------------------------------

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

    (define/override (handle-rst_stream errorcode)
      (when (equal? errorcode error:REFUSED_STREAM) ;; 8.1.4
        (set-received! 'no))
      (cond [(equal? errorcode error:NO_ERROR)
             (case recv-state
               [(done) (void)] ;; Server already sent response independent of data (8.1).
               [else (super handle-rst_stream errorcode)])]
            [else (super handle-rst_stream errorcode)]))

    (define/override (handle-goaway errorcode debug)
      (set-received! 'no) ;; 8.1.4
      (super handle-goaway errorcode debug))

    (define/override (recv-header header)
      (set-received! 'yes)
      (cond [(informational-response? header)
             (log-http2-debug "~a discarding Informational header" (send stream get-ID))
             ;; FIXME: do something with this header??
             #f]
            [else
             (log-http2-debug "~a returning header to user" (send stream get-ID))
             (box-evt-set! resp-bxe (make-response-thunk header))
             #t]))

    (define/override (recv-trailer header-entries)
      (cond [header-entries
             (log-http2-debug "~a returning trailer to user" (send stream get-ID))
             (define trailer-promise (make-header-promise this header-entries "trailer"))
             (box-evt-set! trailerbxe (lambda () (force trailer-promise)))]
            [else
             (box-evt-set! trailerbxe (lambda () #f))]))

    (define/private (make-response-thunk header-entries0)
      ;; Create a promise so that work happens in user thread.
      (define-values (pseudos header-entries) (split-pseudo-header header-entries0))
      (define header-promise (make-header-promise this header-entries "header"))
      (define resp-promise
        (delay/sync
         (define status
           (match (assoc #":status" pseudos)
             [(list* #":status" (and status (regexp #rx#"^[1-5][0-9][0-9]$")) _)
              (string->number (bytes->string/latin-1 status))]
             [_
              (h2-error "bad or missing status from server"
                        #:base-info (get-info-for-exn)
                        #:info (hasheq 'code 'bad-status))]))
         (define header (force header-promise))
         (new http2-response%
              (request req)
              (status-code status)
              (header header)
              (content user-in)
              (trailerbxe trailerbxe))))
      (lambda () (force resp-promise)))

    ;; ------------------------------------------------------------
    ;; Send request header

    (define/override (start*)
      (send-request req))

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
      (define no-content? (or (eq? data #f) (equal? data #"")))
      (when no-content? (close-input-port in-from-user))
      (send stream queue-frames (make-header-frames enc-header no-content?)))

    (define/private (make-header-frames enc-header no-content?)
      (define streamid (send stream get-streamid))
      (define frame-size (send stream get-config-value 'max-frame-size))
      (make-header-frames* enc-header no-content? streamid frame-size))
    ))

(define (make-header-promise app header-entries label)
  (delay/sync
   (with-handler (lambda (e)
                   (h2-error (format "error processing ~a" label)
                             #:base-info (send app get-info-for-exn)
                             #:info (hasheq 'wrapped-exn e)))
     (make-header-from-h2-entries header-entries))))

(define (informational-response? header)
  (match header
    [(cons (list* #":status" (regexp #rx#"^1") _) _) #t]
    [_ #f]))

;; ============================================================

;; FIXME: 8.1.2.2 forbids Connection header, others; filter?
(define (make-pseudo-header method u)
  (list (list #":method" (symbol->bytes method))
        (list #":authority" (url-authority->bytes u)) ;; SHOULD use instead of Host
        (list #":scheme" (string->bytes/utf-8 (url-scheme u)))
        (list #":path" (let ([path (url-path/no-fragment->bytes u)])
                         (if (equal? path #"") #"/" path)))))

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

(define (url-path/no-fragment->bytes u)
  (string->bytes/utf-8
   (url->string (url #f #f #f #f (url-path-absolute? u) (url-path u) (url-query u) #f))))
(define (url-authority->bytes u)
  (string->bytes/utf-8
   (if (url-port u) (format "~a:~a" (url-host u) (url-port u)) (url-host u))))

;; ------------------------------------------------------------

(define (split-pseudo-header entries)
  (let loop ([acc null] [entries entries])
    (match entries
      [(cons (and pseudo (list* (regexp #rx#"^:") _)) rest)
       (loop (cons pseudo acc) rest)]
      [_ (values acc entries)])))
