#lang racket

(provide my-fac)

(define (my-fac)
  (let fac ([n 10])
    (if (equal? 0 n)
      1
      (* n (fac (- n 1))))))
