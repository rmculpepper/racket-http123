#lang racket/base
(require racket/class
         racket/match
         racket/list
         racket/port
         racket/tcp
         net/url-structs
         net/url-string
         binaryio/reader
         openssl
         "interfaces.rkt"
         "header.rkt"
         "regexp.rkt"
         "io.rkt"
         "request.rkt"
         file/gunzip)
(provide (all-defined-out))

;; FIXME: move to response layer

(define PIPE-SIZE 4096)
(define CHUNKED-EOL-MODE 'return-linefeed)
(define CONTENT-LENGTH-READ-NOW (expt 2 20)) ;; FIXME

;; get-decode-mode : Headers -> (U 'gzip 'deflate #f)
(define (get-decode-mode headers)
  (cond [(send headers has-value? 'content-encoding #"gzip") 'gzip]
        [(send headers has-value? 'content-encoding #"deflate") 'deflate]
        [else #f]))

(define (make-decode-wrapper decode-mode out-to-user raise-user-exn)
  (cond [(memq decode-mode '(gzip deflate))
         (define-values (decode-in out-to-decode raise-decode-exn) (make-wrapped-pipe))
         (thread (lambda ()
                   (with-handlers ([exn? (lambda (e) (raise-user-exn e) (raise e))])
                     (case decode-mode
                       [(gzip) (gunzip-through-ports decode-in out-to-user)]
                       [(deflate) (inflate decode-in out-to-user)])
                     (close-output-port out-to-user))))
         (values out-to-decode raise-decode-exn)]
        [else (values out-to-user raise-user-exn)]))

(define (make-decode-input-wrapper decode-mode decode-in)
  (cond [(memq decode-mode '(gzip deflate))
         (define-values (user-in out-to-user raise-user-exn) (make-wrapped-pipe))
         (thread (lambda ()
                   (with-handlers ([exn? (lambda (e) (raise-user-exn e))])
                     (case decode-mode
                       [(gzip) (gunzip-through-ports decode-in out-to-user)]
                       [(deflate) (inflate decode-in out-to-user)]))
                   (close-output-port out-to-user)))
         user-in]
        [else decode-in]))

(define (make-pump decode-mode proc)
  (define-values (wrapped-user-in out-to-user raise-user-exn) (make-wrapped-pipe))
  (define-values (out-to-decode raise-decode-exn)
    (make-decode-wrapper decode-mode out-to-user raise-user-exn))
  (values wrapped-user-in
          (lambda ()
            (with-handlers ([exn? (lambda (e) (raise-decode-exn e) (raise e))])
              (proc out-to-decode)))))

(define (make-pump/content-length br len decode-mode)
  (define (forward/content-length out-to-decode)
    (write-bytes (b-read-bytes br len) out-to-decode)
    (close-output-port out-to-decode))
  (make-pump decode-mode forward/content-length))

(define (make-pump/until-eof in decode-mode)
  (define (forward/until-eof out-to-decode)
    (let loop ()
      (define next (read-bytes PIPE-SIZE in))
      (cond [(eof-object? next)
             (close-input-port in)
             (close-output-port out-to-decode)]
            [else (begin (write-bytes next out-to-decode) (loop))])))
  (make-pump decode-mode forward/until-eof))

(define (make-pump/chunked br decode-mode)
  (define (forward/chunked out-to-decode)
    (define (read-chunk-size)
      (define line (b-read-bytes-line br CHUNKED-EOL-MODE))
      (match (regexp-match #rx"^([0-9a-fA-F]+)(?:$|;)" line) ;; ignore chunk-ext
        [(list _ size-bs) (string->number (bytes->string/latin-1 size-bs) 16)]
        [#f (h-error "expected valid chunk size from server\n  got: ~e" line
                     #:info (hasheq 'received 'yes 'code 'bad-chunked-transfer))]))
    (define (expect-crlf)
      (let ([crlf (b-read-bytes br 2)])
        (unless (equal? crlf #"\r\n")
          (h-error "expected CRLF after chunk\n  received: ~e" crlf
                   #:info (hasheq 'received 'yes 'code 'bad 'chunked-transfer)))))
    (define (read/discard-trailer)
      (define line (b-read-bytes-line br CHUNKED-EOL-MODE))
      (unless (equal? line #"") (read/discard-trailer)))
    (let loop ()
      (define chunk-size (read-chunk-size))
      (cond [(zero? chunk-size)
             (read/discard-trailer)
             (close-output-port out-to-decode)]
            [else
             (define chunk-data (b-read-bytes br chunk-size))
             (write-bytes chunk-data out-to-decode)
             (expect-crlf)
             (loop)])))
  (make-pump decode-mode forward/chunked))
