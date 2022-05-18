#lang racket

(provide fact)
 
(define (fact x)
  (
    (
    (lambda (Y) (Y (lambda (fact) (lambda (n) (if (equal? 0 n) 1 (* n (fact (- n 1))))))))
    (lambda (b) ((lambda (f) (b (lambda (x) ((f f) x))))
                  (lambda (f) (b (lambda (x) ((f f) x))))))
    )
   x)
  )
