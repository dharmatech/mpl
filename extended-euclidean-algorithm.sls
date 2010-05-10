
(library (mpl extended-euclidean-algorithm)

  (export extended-euclidean-algorithm)

  (import (mpl rnrs-sans)
          (mpl arithmetic)
          (mpl leading-coefficient-gpe)
          (mpl algebraic-expand)
          (mpl polynomial-division))

  (define (extended-euclidean-algorithm u v x)

    (if (and (equal? u 0)
             (equal? v 0))

        (list 0 0 0)

        (let loop ((u u)
                   (v v)
                   (App 1)
                   (Ap  0)
                   (A   #f)
                   (Bpp 0)
                   (Bp  1)
                   (B   #f))

          (if (equal? v 0)

              (let ((c (leading-coefficient-gpe u x)))

                (list (algebraic-expand (/ u c))
                      (algebraic-expand (/ App c))
                      (algebraic-expand (/ Bpp c))))

              (let ((q (quotient  u v x))
                    (r (remainder u v x)))

                (let ((A (- App (* q Ap)))
                      (B (- Bpp (* q Bp))))

                  (loop v r Ap A A Bp B B))))))))