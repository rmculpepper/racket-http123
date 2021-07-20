#lang racket/base
(require racket/runtime-path
         web-server/servlet
         web-server/servlet-env)

(define-values (dispatch _make-url)
  (dispatch-rules
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
   ))

;; --

(define-runtime-path static-dir "static")

;; Go
(serve/servlet dispatch
               #:port 17080
               #:servlet-regexp #rx""
               #:command-line? #t
               ;; #:launch-browser? #f
               #:extra-files-paths (list (path->string static-dir))
               #:log-file "/dev/stdout")
