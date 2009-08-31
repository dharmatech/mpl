
(library (mpl algebraic-expand)

  (export algebraic-expand)

  (import (except (rnrs) + - * /)
          (mpl misc)
          (mpl automatic-simplification)
          (mpl expand-product)
          (mpl expand-power)
          )

  (define (algebraic-expand u)

    (cond ( (sum? u)

            (let ((v (list-ref u 1)))

              (+ (algebraic-expand v)

                 (algebraic-expand (- u v)))) )

          ( (product? u)

            (let ((v (list-ref u 1)))

              (expand-product (algebraic-expand v)
                              (algebraic-expand (/ u v)))) )

          ( (power? u)

            (let ((base     (list-ref u 1))
                  (exponent (list-ref u 2)))

              (if (and (integer? exponent)
                       (>= exponent 2))

                  (expand-power (algebraic-expand base)
                                exponent)

                  u)) )

          ( else u )))

  )