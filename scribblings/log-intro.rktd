;; This file was created by make-log-based-eval
((require http123
          racket/class
          racket/port
          racket/pretty
          (submod http123/private/util pretty))
 ((3) 0 () 0 () () (c values c (void)))
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
     (send h remove! 'alt-svc)
     (send h remove! 'set-cookie)))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((pretty (call-with-output-string (lambda (out) (pretty-print resp out))))
 ((3)
  1
  (((submod (lib "http123/private/util.rkt") pretty)
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
     "(new http2-response%\n (status-code 200)\n (header\n  (new header%\n   (table\n    '#hasheq((cache-control . #\"private, max-age=0\")\n             (content-encoding . #\"gzip\")\n             (content-length . #\"6244\")\n             (content-type . #\"text/html; charset=ISO-8859-1\")\n             (date . #\"Sun, 25 Apr 2021 02:49:11 GMT\")\n             (expires . #\"-1\")\n             (p3p\n              .\n              #\"CP=\\\"This is not a P3P policy! See g.co/p3phelp for more info.\\\"\")\n             (server . #\"gws\")\n             (x-frame-options . #\"SAMEORIGIN\")\n             (x-xss-protection . #\"0\")))))\n ...)\n"))))
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
