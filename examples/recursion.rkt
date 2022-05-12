#lang racket

(provide add-fact)

(define (add-fact x y) 
  (letrec ([fact (lambda (n)
                  (if (equal? 0 n) 1 (* n (fact (- n 1)))))])
    (+ (fact x) (fact y))))
