(module arithmetic racket
  (provide a1 a2 a3 a4 a5)
  (define a1 (+ (/ 10 (* 1.5 3)) (* 23 10)))
  (define (a2) (* (+ 12 (/ 12 4)) (- 25 10)))
  (define (a3 x) (* (+ x (/ 12 4)) (- 25 10)))
  (define (a4 x y) (* (+ 12 (/ y 4)) (- x 10)))
  (define (a5 x y z) (* (+ z (/ x 4)) (- y 10))))


