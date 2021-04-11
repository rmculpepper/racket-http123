#lang racket/base
(require racket/match)
(provide (all-defined-out))

;; A Request is:
(struct request
  (method       ;; Symbol, like 'GET
   url          ;; URL
   headers      ;; (Listof Bytes)
   data         ;; (U Bytes (Bytes -> Void) 'removed)
   ) #:prefab)

;; request:can-replay? : Request -> Boolean
;; Can the request be replayed in a different actual connection?  Only care
;; about request semantics, not effect on connection state (eg close, upgrade).
(define (request:can-replay? req)
  ;; FIXME: depends on policy, belongs in connection?
  (match req
    [(request method url headers data)
     (and (method:idempotent? method)
          (small-data? data))]))

(define SMALL-DATA-LIMIT (expt 2 24))

(define (small-data? data)
  (or (eq? data #f)
      (and (bytes? data) (<= (bytes-length data) SMALL-DATA-LIMIT))))

;; request:copy-for-queue : Request -> Request
;; Make a copy w/o data for the recv queue.
(define (request:copy-for-queue req)
  (match req
    [(request method url headers (? small-data? data))
     req]
    [(request method url headers data)
     (request method url headers 'removed)]))

;; ----------------------------------------

;; RFC 7231 4.2.1 (Safe Methods)
(define (method:safe? method)
  (memq method '(GET HEAD OPTIONS TRACE)))

;; RFC 7231 4.2.? (Idempotent Methods)
(define (method:idempotent? method)
  (or (method:safe? method) (memq method '(PUT DELETE))))
