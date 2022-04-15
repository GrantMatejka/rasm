(module add racket
  (provide f x)
  
  (define f
    (let ((y 5))
      (lambda (x) (+ x y))))
      
  (define x (f 2)))
