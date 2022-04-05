(module unique racket
  (define test 
    (lambda () (let ((x 10) (y 9) (z 20)) (+ x (+ y z))))))