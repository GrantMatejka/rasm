(module add racket
  (provide zero?)
  (provide even?)
  
  (define zero? (lambda (x) (if (equal? 0 x) #t #f)))
  
  (define one? (lambda (x) (if (equal? 1 x) #t #f))))
