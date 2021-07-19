;; This file was created by make-log-based-eval
((require http123
          racket/class
          racket/port
          racket/pretty
          json
          (submod http123/scribblings/util pretty))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define client
   (http-client
    #:add-header
    `((x-racket-version ,(version)))
    #:add-content-handlers
    `((application/json ,read-json))))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define time-req (request 'GET "http://date.jsontest.com/"))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((send client handle time-req)
 ((3)
  0
  ()
  0
  ()
  ()
  (c
   values
   c
   (h
    -
    ()
    (time u . "07:19:32 PM")
    (date u . "07-19-2021")
    (milliseconds_since_epoch . 1626722372856))))
 #""
 #"")
((send client handle (request 'GET "https://tools.ietf.org/rfc/rfc7540.txt"))
 ((3)
  0
  ()
  0
  ()
  ()
  (q
   exn
   "handle: no content handler matched\n  code: 'unhandled-content\n  response: 200 response with text/plain body\n  received: 'yes"))
 #""
 #"")
((define client2
   (send client fork
     #:add-content-handlers
     (quasiquote
      ((text/plain ,(lambda (in) (read-string 40 in)))
       (*/*
        ,(lambda (in)
           (format
            "something called ~s, I guess"
            (send (current-response) get-content-type))))))))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((send client2 handle (request 'GET "https://tools.ietf.org/rfc/rfc7540.txt"))
 ((3)
  0
  ()
  0
  ()
  ()
  (c values c (u . "\n\n\n\n\n\nInternet Engineering Task Force (I")))
 #""
 #"")
((send client2 handle (request 'GET "https://www.google.com/"))
 ((3) 0 () 0 () () (c values c (u . "something called text/html, I guess")))
 #""
 #"")
((send client2 handle
   (request 'GET "https://mirror.racket-lang.org/no-such-file.html"))
 ((3)
  0
  ()
  0
  ()
  ()
  (q
   exn
   "handle: no response handler matched\n  code: 'unhandled-response\n  response: 404 response with text/html body\n  received: 'yes"))
 #""
 #"")
((define client3
   (send client2 fork
     #:add-response-handlers
     (quasiquote
      ((404 ,(lambda (client resp) 'not-found))
       (client-error ,(lambda (client resp) 'failed))))))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((send client3 handle
   (request 'GET "https://racket-lang.org/secret-plans.scrbl"))
 ((3) 0 () 0 () () (q values failed))
 #""
 #"")
((send client3 handle
   (request 'GET "https://mirror.racket-lang.org/no-such-file.html"))
 ((3) 0 () 0 () () (q values not-found))
 #""
 #"")
((define client4
   (send client3 fork
     #:add-response-handlers
     (quasiquote
      ((404
        ,(lambda (client resp)
           (list 'not-found (send client handle-response-content resp))))))))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((send client4 handle
   (request 'GET "https://mirror.racket-lang.org/no-such-file.html"))
 ((3)
  0
  ()
  0
  ()
  ()
  (c values c (c not-found c (u . "something called text/html, I guess"))))
 #""
 #"")
((define client (http-client)) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((define header
   '("Accept-Encoding: gzip, deflate"
     (accept-language #"en")
     (#"User-Agent" #"racket-http123/0.1")))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define req (request 'GET "https://www.google.com/" header #f))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define resp (send client sync-request req))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((begin
   (let ((h (send resp get-header)))
     (send h remove! #"alt-svc")
     (send h remove! #"set-cookie")))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((pretty (call-with-output-string (lambda (out) (pretty-print resp out))))
 ((3)
  1
  (((submod (lib "http123/scribblings/util.rkt") pretty)
    .
    deserialize-info:pretty-v0))
  0
  ()
  ()
  (c
   values
   c
   (0
    (u
     .
     "(new http2-response%\n (status-code 200)\n (header\n  (new header%\n   (header-fields\n    '((#\"date\" #\"Mon, 19 Jul 2021 19:19:39 GMT\")\n      (#\"expires\" #\"-1\")\n      (#\"cache-control\" #\"private, max-age=0\")\n      (#\"content-type\" #\"text/html; charset=ISO-8859-1\")\n      (#\"p3p\"\n       #\"CP=\\\"This is not a P3P policy! See g.co/p3phelp for more info.\\\"\")\n      (#\"content-encoding\" #\"gzip\")\n      (#\"server\" #\"gws\")\n      (#\"content-length\" #\"6150\")\n      (#\"x-xss-protection\" #\"0\")\n      (#\"x-frame-options\" #\"SAMEORIGIN\")))))\n ...)\n"))))
 #""
 #"")
((send resp get-status-code) ((3) 0 () 0 () () (q values 200)) #"" #"")
((read-string 15 (send resp get-content-in))
 ((3) 0 () 0 () () (c values c (u . "<!doctype html>")))
 #""
 #"")
((read-string 5 (send resp get-content-in))
 ((3) 0 () 0 () () (c values c (u . "<html")))
 #""
 #"")
((define ietf-evt
   (send client async-request
     (request 'GET "https://tools.ietf.org/rfc/rfc7540.txt")))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((define google-evt
   (send client async-request (request 'GET "https://www.google.com/")))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((let ((resp ((sync ietf-evt google-evt))))
   (define h (send resp get-header))
   (send h remove! #"alt-svc")
   (send h remove! #"set-cookie")
   (pretty (call-with-output-string (lambda (out) (pretty-print resp out)))))
 ((3)
  1
  (((submod (lib "http123/scribblings/util.rkt") pretty)
    .
    deserialize-info:pretty-v0))
  0
  ()
  ()
  (c
   values
   c
   (0
    (u
     .
     "(new http2-response%\n (status-code 200)\n (header\n  (new header%\n   (header-fields\n    '((#\"date\" #\"Mon, 19 Jul 2021 19:19:40 GMT\")\n      (#\"expires\" #\"-1\")\n      (#\"cache-control\" #\"private, max-age=0\")\n      (#\"content-type\" #\"text/html; charset=ISO-8859-1\")\n      (#\"p3p\"\n       #\"CP=\\\"This is not a P3P policy! See g.co/p3phelp for more info.\\\"\")\n      (#\"content-encoding\" #\"gzip\")\n      (#\"server\" #\"gws\")\n      (#\"content-length\" #\"5864\")\n      (#\"x-xss-protection\" #\"0\")\n      (#\"x-frame-options\" #\"SAMEORIGIN\")))))\n ...)\n"))))
 #""
 #"")
