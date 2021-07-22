#lang racket/base
(require "private/interfaces.rkt"
         "private/connection.rkt")
(provide http-client-base<%>
         (rename-out [connection-manager% http-connection-manager%])
         http-connection%
         http-actual-connection<%>)
