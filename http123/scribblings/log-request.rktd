;; This file was created by make-log-based-eval
((require http123
          racket/class
          racket/port
          racket/pretty
          net/url-string
          (submod http123/scribblings/util pretty))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((P
  (request
   'HEAD
   "https://blog.racket-lang.org/"
   '("If-Modified-Since: Sun, 14 Feb 2021 01:00:00 GMT")))
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
     "(request\n 'HEAD\n (url \"https\" #f \"blog.racket-lang.org\" #f #t (list (path/param \"\" '())) '() #f)\n '((#\"if-modified-since\" #\"Sun, 14 Feb 2021 01:00:00 GMT\"))\n #f)\n"))))
 #""
 #"")
((ok-http-url? (string->url "http://racket-lang.org/"))
 ((3) 0 () 0 () () (q values #t))
 #""
 #"")
((ok-http-url? (string->url "http://ryanc@racket-lang.org/"))
 ((3) 0 () 0 () () (q values #f))
 #""
 #"")
((ok-http-url? (string->url "ftp://mirror.racket-lang.org/"))
 ((3) 0 () 0 () () (q values #f))
 #""
 #"")
((ok-http-url? (string->url "somewhere/out-there.html"))
 ((3) 0 () 0 () () (q values #f))
 #""
 #"")
