(module add racket
  (provide five?)
  (provide one?)
  
  (define five? (lambda (x) (if (equal? x 5) #t #f)))
  
  (define one? (lambda (x) (if (equal? 1 x) #t #f))))
