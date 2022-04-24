(module add racket
  (provide f x)
  
  (define f
    (let ((y 5))
      ((lambda (x) (+ x y)) 3)))
      
  (define x (lambda () f)))
