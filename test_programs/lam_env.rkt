(module add racket
  (provide f)
  
  (define f
    (let ((y 5))
      ((lambda (x) (+ x y)) 3))))
