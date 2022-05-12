#lang racket

(provide apply-adder make-adder)

(define (apply-adder adder x)
  (adder x))

(define (make-adder v) (lambda (x) (+ v x)))

