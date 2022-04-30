(module nested_arithmetic racket
  (provide add1 sub1 mul1 div1
           add2 sub2 mul2 div2
           add3 sub3 mul3 div3)
  (define (add1) (+ 41234 (+ (+ 4508 5000) 10)))
  (define (add2) (+ (+ 4.5 6.7) (+ 10.2 78.3)))
  (define (add3) (+ (+ 6.7 4.5) (+ 4 50)))
  (define (sub1) (- 123435 (- (- 35 7) 3)))
  (define (sub2) (- (- 2.5 1.3) (- 1.2 0.7)))
  (define (sub3) (- (- 2.5 0.45) (- 1 45)))
  (define (mul1) (* 100 (* 15 (* 80 3))))
  (define (mul2) (* (* 1.3 2.5) (* 0.5 10.5)))
  (define (mul3) (* (* 2.1 7.5) (* 8.99 10)))
  (define (div1) (/ (/ 4500 5) (/ 4 2)))
  (define (div2) (/ (/ 450.5 0.9) 9.5))
  (define (div3) (/ (/ 2.5 0.5) (/ (/ 10 2) 2))))


