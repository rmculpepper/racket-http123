#lang racket/base
(require racket/class
         racket/contract/base
         racket/match
         racket/port
         "interfaces.rkt"
         "header.rkt"
         "decode.rkt")
(provide (all-defined-out))

(define http-response<%>
  (interface ()
    [get-status-code
     (->m exact-nonnegative-integer?)]
    [get-status-class
     (->m symbol?)]
    [get-header
     (->m (is-a?/c header<%>))]
    [get-content
     (->m (or/c #f bytes? input-port?))]
    [get-trailer
     (->m (or/c #f (is-a?/c header<%>)))]
    [get-trailer-evt
     (->m (evt/c (-> (or/c #f (is-a?/c header<%>)))))]
    ))

;; ------------------------------------------------------------

(define http-response%
  (class* object% (http-response<%> class-printable<%>)
    (init-field status-code     ;; Nat
                header          ;; header%
                trailerbxe)     ;; (Evt (-> (or/c #f header%)))
    (init ((init-content content))) ;; #f or Bytes or InputPort
    (super-new)

    ;; content : #f or Bytes
    (field [content (and (bytes? init-content) init-content)])

    ;; content-in : #f or InputPort
    (field [content-in (and (input-port? init-content) init-content)])

    (define/public (get-status-code) status-code)
    (define/public (get-status-class)
      (status-code->class status-code))
    (define/public (get-header) header)

    (define/public (has-content?) (or content content-in))

    (define/public (get-content)
      (or content
          (and content-in
               (let ([c (port->bytes content-in)])
                 (begin (set! content c) c)))))

    (define/public (get-content-in)
      (or content-in
          (and content
               (let ([ci (open-input-bytes content)])
                 (begin (set! content-in ci) ci)))))

    (abstract get-version)

    (define/public (get-trailer-evt)
      (or trailerbxe const-false-evt))
    (define/public (get-trailer)
      (and trailerbxe (sync trailerbxe)))

    ;; ----

    (define/public (get-printing-classname)
      'http-response%)
    (define/public (get-printing-components)
      (cond [(not content-in)
             (values '(status-code header content)
                     (list status-code header content)
                     #t)]
            [else
             (values '(status-code header content-in)
                     (list status-code header content-in)
                     #t)]))
    ))

(define const-false-evt
  (let ([const-false (lambda () #f)])
    (wrap-evt always-evt (lambda (ae) const-false))))

;; ----------------------------------------

(define decoding-response%
  (class http-response%
    (init header
          content
          [handle-content-encoding? #t])
    (let ()
      (define decode-mode (get-decode-mode header))
      (define content*
        (case (and content decode-mode)
          [(gzip deflate)
           (let ([content-in (if (bytes? content) (open-input-bytes content) content)])
             (make-decode-input-wrapper decode-mode content-in))]
          [else content]))
      (super-new (header header) (content content*)))
    ))

;; ----------------------------------------

(define http11-response%
  (class* decoding-response% ()
    (init-field [status-line #f])
    (super-new)

    (define/override (get-version) 'http/1.1)
    (define/public (get-status-line) status-line)
    (define/override (get-printing-classname) 'http11-response%)
    ))

(define http2-response%
  (class* decoding-response% ()
    (super-new)
    (define/override (get-version) 'http/2)
    (define/override (get-printing-classname) 'http2-response%)
    ))

;; ============================================================
;; Status Codes

;; Reference:
;; - https://tools.ietf.org/html/rfc7231#section-6

(define (status-code->class status-code)
  (cond [(<= 100 status-code 199) 'informational]
        [(<= 200 status-code 299) 'successful]
        [(<= 300 status-code 399) 'redirection]
        [(<= 400 status-code 499) 'client-error]
        [(<= 500 status-code 599) 'server-error]))

(define (status-code-cacheable-by-default? status-code) ;; 6.1
  (and (memq status-code '(200 203 204 206 300 301 404 405 410 414 501)) #t))

(define (status-code-with-content? status-code) ;; 3.3.3
  ;; #t means might have content; #f means certainly does not
  ;; Also, response to HEAD request.
  (cond [(<= 100 status-code 199) #f]
        [(memv status-code '(204 304)) #f]
        [else #t]))

(define (status-code-reason code)
  (case code
    [(100) "Continue"]
    [(101) "Switching Protocols"]
    [(200) "OK"]
    [(201) "Created"]
    [(202) "Accepted"]
    [(203) "Non-Authoritative Information"]
    [(204) "No Content"]
    [(205) "Reset Content"]
    [(206) "Partial Content"]
    [(300) "Multiple Choices"]
    [(301) "Moved Permanently"]
    [(302) "Found"]
    [(303) "See Other"]
    [(304) "Not Modified"]
    [(305) "Use Proxy"]
    [(307) "Temporary Redirect"]
    [(400) "Bad Request"]
    [(401) "Unauthorized"]
    [(402) "Payment Required"]
    [(403) "Forbidden"]
    [(404) "Not Found"]
    [(405) "Method Not Allowed"]
    [(406) "Not Acceptable"]
    [(407) "Proxy Authentication Required"]
    [(408) "Request Timeout"]
    [(409) "Conflict"]
    [(410) "Gone"]
    [(411) "Length Required"]
    [(412) "Precondition Failed"]
    [(413) "Payload Too Large"]
    [(414) "URI Too Long"]
    [(415) "Unsupported Media Type"]
    [(416) "Range Not Satisfiable"]
    [(417) "Expectation Failed"]
    [(426) "Upgrade Required"]
    [(500) "Internal Server Error"]
    [(501) "Not Implemented"]
    [(502) "Bad Gateway"]
    [(503) "Service Unavailable"]
    [(504) "Gateway Timeout"]
    [(505) "HTTP Version Not Supported"]
    [else #f]))
