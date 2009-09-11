
(library (mpl expand-product)

  (export expand-product)

  (import (mpl rnrs)
          (mpl misc))

  (define (expand-product r s)

    (cond ( (sum? r)

            (let ((f (list-ref r 1)))

              (+ (expand-product f s)

                 (expand-product (- r f) s))) )

          ( (sum? s) (expand-product s r) )

          ( else (* r s) )))

  )