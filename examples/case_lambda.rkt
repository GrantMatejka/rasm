#lang racket

(provide f cl1 cl2 cl3 cl4)

(define f (case-lambda
             [() 0]
             [(x) (+ 1 x)]
             [(x y) (+ x y)]
             [(x y z) (+ z (+ x y))]))

(define (cl1) (f))

(define (cl2 x) (f x))

(define (cl3 x y) (f x y))

(define (cl4 x y z) (f x y z))

