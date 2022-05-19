#lang racket

(provide g a)

(define (g x) (lambda () x))

(define a (g 1))

