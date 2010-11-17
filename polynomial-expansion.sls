#!r6rs

(library (mpl polynomial-expansion)

  (export polynomial-expansion)

  (import (mpl rnrs-sans)
          (mpl arithmetic)
          (mpl algebraic-expand)
          (mpl polynomial-division))

  (define (polynomial-expansion u v x t)
    (if (equal? u 0)
        0
        (let ((d (polynomial-division u v x)))
          (let ((q (list-ref d 0))
                (r (list-ref d 1)))
            (algebraic-expand (+ (* t (polynomial-expansion q v x t))
                                 r)))))))
