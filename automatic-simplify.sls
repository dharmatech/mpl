
(library (mpl automatic-simplify)

  (export automatic-simplify)

  (import (except (rnrs) + - * / exp sin cos)
          (mpl misc)
          (mpl automatic-simplification)
          )

  (define (automatic-simplify u)
    (if (list? u)
        (let ((v (map automatic-simplify u)))
          (cond ((power?      v) (simplify-power      v))
                ((product?    v) (simplify-product    v))
                ((sum?        v) (simplify-sum        v))
                ((quotient?   v) (simplify-quotient   v))
                ((difference? v) (simplify-difference v))
                ((factorial?  v) (simplify-factorial  v))
                (else                                 v)))
        u))

  )