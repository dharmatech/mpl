
(library (mpl expand-main-op)

  (export expand-main-op)

  (import (rnrs)
          (mpl misc)
          (mpl expand-product)
          (mpl expand-power))

  (define (expand-main-op u)

    (cond ( (product? u)

            (expand-product (list-ref u 1)
                            (list-ref u 2)) )

          ( (power? u)

            (expand-power (list-ref u 1)
                          (list-ref u 2)) )

          ( else u ))))