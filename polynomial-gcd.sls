
(library (mpl polynomial-gcd)

  (export polynomial-gcd)

  (import (mpl rnrs-sans)
          (mpl arithmetic)
          (mpl algebraic-expand)
          (mpl leading-coefficient-gpe)
          (mpl polynomial-division))

  (define (polynomial-gcd u v x)
    (if (and (equal? u 0)
             (equal? v 0))
        0
        (let loop ((u u) (v v))
          (if (equal? v 0)
              (algebraic-expand
               (* (/ 1 (leading-coefficient-gpe u x))
                  u))
              (loop v (remainder u v x)))))))
