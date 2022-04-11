(module test racket
  (provide f)
  
  (define (f x)
    (define y 5)
    (define my-add (lambda (x y) (+ x y)))
    (+ (my-add x y) (my-add x 10))))
