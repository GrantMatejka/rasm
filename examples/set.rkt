#lang racket

(provide x f g)

(define x 1)

(define (f) (begin
  (set! x (+ x 1))
  x))

(define (g) (begin (set! x 5) x))


