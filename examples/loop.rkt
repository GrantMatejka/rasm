#lang racket

(provide my-loop)

(define (my-loop x)
  (let loop ([x x])
    (if (< x 10)
      (loop (+ 1 x))
      x)))
