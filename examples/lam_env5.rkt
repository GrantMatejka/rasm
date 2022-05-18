(module ycomb racket

  (provide f)

  (define (f x)
    (
     (
      (lambda (sub1) (lambda (add1) (sub1 (add1 x))))
      (lambda (x) (- x 1))
      )
     (lambda (x) (+ x 1))
     )
    )
  )