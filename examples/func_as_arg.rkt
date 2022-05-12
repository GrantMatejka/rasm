#lang racket

(provide func)

(define (func x)
  ((lambda (x y) (x y))
   (lambda (x) (+ 1 x))
   x))
