(module ycomb racket

  (provide fact)

  (define (fact x)
    (
      (
        (lambda (Y) (Y (lambda (f) (lambda (n) (if (equal? 0 n) 1 (* n (f (- n 1))))))))
        (
          (lambda {x} (lambda {y} (y (lambda {z} (((x x) y) z)))))
          (lambda {x} (lambda {y} (y (lambda {z} (((x x) y) z)))))
        )
      )
    x)
  )
)
