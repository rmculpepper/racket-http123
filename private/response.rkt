#lang racket/base
(require racket/class
         racket/match
         racket/struct
         "interfaces.rkt"
         "decode.rkt")
(provide (all-defined-out))

(struct print:init (name value)
  #:property prop:custom-write
  (make-constructor-style-printer
   (lambda (self) (print:init-name self))
   (lambda (self) (list (print:init-value self)))))

(define class-printable<%>
  (interface* () ([prop:custom-write
                   (let ()
                     (define ((emit-fields make-init) self)
                       (define-values (fieldnames fieldvals more?)
                         (send self get-printing-components))
                       (append (for/list ([fieldname (in-list fieldnames)]
                                          [fieldval (in-list fieldvals)])
                                 (make-init fieldname fieldval))
                               (if more? (list (unquoted-printing-string "...")) '())))
                     (define (emit-new-classname self)
                       (string->symbol (format "new ~a" (send self get-printing-classname))))
                     (define (emit-classname self)
                       (string->symbol (format "~a" (send self get-printing-classname))))
                     (define writer
                       (make-constructor-style-printer emit-classname (emit-fields list)))
                     (define printer
                       (make-constructor-style-printer emit-new-classname (emit-fields print:init)))
                     (lambda (self out mode)
                       (case mode
                         [(#t #f) (writer self out mode)]
                         [else (printer self out mode)])))])
    get-printing-classname
    get-printing-components))

(define class-never-quotable<%>
  (interface* () ([prop:custom-print-quotable 'never])))

;; ------------------------------------------------------------

(define http-response%
  (class* object% (class-printable<%>)
    (init-field status-code     ;; Nat
                headers         ;; headers%
                content)        ;; Bytes or InputPort
    (init [handle-content-encoding? #t])
    (super-new)

    (define/public (get-status-code) status-code)
    (define/public (get-status-class)
      (status-code->class status-code))
    (define/public (get-headers) headers)
    (define/public (get-content) content)
    (abstract get-version)

    ;; ----

    (define content-decoded #f)
    (when handle-content-encoding?
      (set! content-decoded (handle-content-encoding)))

    (define/public (get-content-encoding)
      ;; FIXME: could extend to Content-Encoding lists, remove known
      (cond [content-decoded #f]
            [else (send headers get-value 'content-encoding)]))

    (define/public (handle-content-encoding)
      (define decode-mode (get-decode-mode headers))
      (define (get-content-in)
        (if (bytes? content) (open-input-bytes content) content))
      (case decode-mode
        [(gzip deflate)
         (eprintf "!! decoding ~s\n" decode-mode)
         (set! content
               (make-decode-input-wrapper decode-mode (get-content-in)))
         decode-mode]
        [else #f]))

    (define/public (get-printing-classname)
      'http-response%)
    (define/public (get-printing-components)
      (values '(status-code headers content)
              (list status-code headers content)
              #t))
    ))

;; ----------------------------------------

(define http11-response%
  (class* http-response% ()
    (init-field [status-line #f])
    (super-new)

    (define/override (get-version) 'http/1.1)
    (define/public (get-status-line) status-line)
    ))

(define http2-response%
  (class* http-response% ()
    (super-new)
    (define/override (get-version) 'http/2)
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
