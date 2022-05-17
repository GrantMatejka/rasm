#lang racket

(provide my-loop)

(define (my-loop x)
  (let loop ([x x])
    (if (> x 0)
      (loop (- 1 x))
      x)))
