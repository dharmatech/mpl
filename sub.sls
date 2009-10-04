
(library (mpl sub)

  (export - simplify-difference)

  (import (except (rnrs) + * -)
          (xitomatl AS-match)
          (mpl sum-product-power))

  (define (simplify-difference u)
    (match u
      ( ('- x)   (* -1 x) )
      ( ('- x y) (+ x (* -1 y)) )))

  (define (- . elts)
    (simplify-difference `(- ,@elts)))

  )