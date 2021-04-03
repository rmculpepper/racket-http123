#lang racket/base
(require racket/class
         "interfaces.rkt")
(provide (all-defined-out))

;; Local conventions
;; - loc : url or string
;; - u   : url

(define http-base%
  (class object% (http<%>)
    (super-new)

    (define/public (do-request/verb verb loc headers
                                    #:handler handler)
      (define u (coerce-url loc))
      (define conn (connect/url u))
      (define-values (response-line response-headers response-content-in)
        (send conn do-request/verb verb loc headers))
      (handler response-line response-headers response-content-in))

    (define/public (request/verb verb loc
                                 #:handler [content-handler void]
                                 #:meta-handler [meta-handler (lambda (response) values)])
      (define response ___)
      (define wrapper (meta-handler response))
      (call-with-values
       (lambda () (send response call/content-input-port content-handler))
       wrapper))

    (define-syntax-rule (define-method name)
      (define/public (name loc
                           #:handler [content-handler void]
                           #:meta-handler [meta-handler (lambda (response) values)])
        (with-entry-point 'name
          (request/verb 'name loc
                        #:handler content-handler
                        #:meta-handler meta-handler))))

    (define-method GET)
    (define-method HEAD)
    (define-method POST)
    (define-method PUT)
    (define-method DELETE)
    ))


(define http-response-base%
  (class object% (http-response<%>)
    (init-field response-line       ;; Bytes
                headers             ;; ???
                content-input-port  ;; InputPort or #f
                content)
    (super-new)

    ;; WAIT: try to avoid this stateful nonsense...

    (define/public (call/content-input-port proc)
      (cond [content-input-port
             => (lambda (in)
                  (set! content-input-port #f)
                  (set! content (proc in))
                  ___ clean up in ___
                  content)]
            [else (error 'call/content-input-port "already called")]))

    ))



#|
(send hc GET
      uri
      #:meta-handler (lambda (response)
                       (define etag (send response get-header "ETag"))
                       (lambda (v) (values etag v)))
      #:handler (lambda (in) (read-json in)))

response-handler =
(lambda (response)
  (define wrapper (meta-handler response))
  (call-with-values
   (lambda () (handler (send response get-content-input-port)))
   wrapper))
|#
