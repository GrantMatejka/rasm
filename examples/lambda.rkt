#lang racket

(provide func)


(define (func)
  (((lambda (x) (lambda () (+ 15 (x))))
   (lambda () 12))))
