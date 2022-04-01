(module add racket
  (provide mycond)
  
  (define mycond (lambda (x)
                   (cond
                     [(< x 0) 0]
                     [(< x 5) 1]
                     [(< x 10) 2]
                     [else 3]))))
