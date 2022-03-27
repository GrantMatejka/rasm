(module add racket
  (provide add1)
  (provide seven)
  (provide double)

  (define tmp 3)
  
  (define seven (+ 3 4))

  (define add1 (lambda (x) (+ 1 x)))

  (define sub1 (lambda (x) (- x 1)))

  (define half (lambda (x) (/ x 2)))

  (define double (lambda (x) (+ x x)))

  (define square (lambda (x) (* x x)))
  
  (define add (lambda (x y) (+ x y))))
