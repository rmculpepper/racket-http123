#lang racket/base
(require racket/runtime-path
         web-server/servlet
         web-server/servlet-env
         json)

(define-values (dispatch _make-url)
  (dispatch-rules
   [("hello-text") #:method "get"
    (lambda (req) (ok-response #"text/plain" "hello world"))]
   ;; ----
   [("echo") #:method "post"
    (lambda (req)
      (define content-type
        (cond [(headers-assq* #"content-type" (request-headers/raw req)) => header-value]
              [else #"content-type" #"text/unknown"]))
      (define body (request-post-data/raw req))
      (response/full
       200 #"Found"
       (current-seconds) content-type
       null
       (list body)))]
   [("lots") #:method "post"
    (lambda (req)
      (define body (request-post-data/raw req))
      (define n (read (open-input-bytes body)))
      (response/output
       #:code 200
       #:mime-type #"text/plain"
       (lambda (out) (for ([i n]) (write-char #\a out)))))]
   [("secret") #:method "get"
    (lambda (req)
      (response/output
       #:code 403
       #:mime-type #"text/plain"
       (lambda (out) (fprintf out "go away!"))))]
   [("redirect301") #:method "post"
    (lambda (req)
      (response/output
       #:code 301
       #:headers (list (header #"Location" (request-post-data/raw req)))
       void))]
   [("form2json") #:method "post"
    (lambda (req)
      (response/output
       #:code 200
       #:mime-type #"application/json"
       (lambda (out)
         (write-json (for/hash ([b (request-bindings/raw req)] #:when (binding:form? b))
                       (values (string->symbol (bytes->string/utf-8 (binding-id b)))
                               (bytes->string/utf-8 (binding:form-value b))))
                     out))))]
   ))

(define (ok-response content-type data)
  (response/output
   #:code 200
   #:mime-type content-type
   (lambda (out)
     (cond [(string? data) (write-string data out)]
           [(procedure? data) (data out)]))))

;; --

(define-runtime-path static-dir "static")

;; Go
(serve/servlet dispatch
               #:port 17180
               #:servlet-regexp #rx""
               #:command-line? #t
               ;; #:launch-browser? #f
               #:extra-files-paths (list (path->string static-dir))
               #:log-file "/dev/stdout")
