#lang racket

(provide fact fact2)

(define Y (lambda (b) ((lambda (f) (b (lambda (x) ((f f) x))))
                       (lambda (f) (b (lambda (x) ((f f) x)))))))
 
(define (fact x)
  ((Y (lambda (fact) (lambda (n) (if (equal? 0 n) 1 (* n (fact (- n 1))))))) x))

 
(define fact2
  (Y (lambda (fact) (lambda (n) (if (equal? 0 n) 1 (* n (fact (- n 1))))))))
