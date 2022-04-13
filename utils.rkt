#lang typed/racket

(provide (all-defined-out))


; Whether we have an anonymous lambda or not
(define (anon? [fn : Symbol]) : Boolean
  (string-contains? (~a fn) "__lambda"))
