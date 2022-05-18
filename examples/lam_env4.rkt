#lang racket

(provide p)

(define p ((lambda (x) (lambda (n) (+ n x))) 10))
