#lang racket

(provide ycomb)

(define (ycomb dc)
  ((lambda
       {empty}
     ((lambda
          {cons}
        ((lambda
             {empty?}
           ((lambda
                {first}
              ((lambda
                   {rest}
                 ((lambda
                      {Y}
                    ((lambda
                         {length}
                       ((lambda {addup} (addup (cons 3 (cons 17 empty))))
                        (Y
                         (lambda
                             {addup}
                           (lambda {l} (if (empty? l) 0 (+ (first l) (addup (rest l)))))))))
                     (Y
                      (lambda
                          {length}
                        (lambda {l} (if (empty? l) 0 (+ 1 (length (rest l)))))))
                     ))
                  ((lambda {x} (lambda {y} (y (lambda {z} (((x x) y) z)))))
                   (lambda {x} (lambda {y} (y (lambda {z} (((x x) y) z))))))))
               (lambda {l} (l false))))
            (lambda {l} (l true))))
         (lambda {l} (equal? l empty))))
      (lambda {a b} (lambda {select} (if select a b)))))
   13)
  )
