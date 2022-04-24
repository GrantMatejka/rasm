(module add racket
  (provide add a)

  (define (make-adder x)
    (λ (y) (+ x y)))
  (define a (make-adder 19))
  ;; should be 22
  (define (add) (a 3)))
