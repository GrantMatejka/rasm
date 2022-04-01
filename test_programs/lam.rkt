(module add racket
  (provide triple)
  
  (define triple (lambda (x) (+ ((lambda (x) (+ x x)) x) x))))
