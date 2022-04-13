(module my-let racket
  (provide test)

  (define test 
    (lambda () (let ((x 10) (y 9) (z 20)) (+ x (+ y z))))))