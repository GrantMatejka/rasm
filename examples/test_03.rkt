#lang racket

(provide my-fac)

(define (my-fac x) 
  (let fac ([n x])
    (if (equal? 0 n)
      1
      (* n (fac (- n 1))))))
