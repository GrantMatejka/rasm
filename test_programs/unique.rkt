(module unique racket
  (define test 
    (lambda () (let ((x 10))
      (+ x (let ((x 23)) x))))))