(module unique racket
  (provide test)
  
  (define test 
    (lambda () (let ((x 10))
      (+ x (let ((x 23)) x))))))