
(library (mpl div)

  (export / simplify-quotient)

  (import (except (rnrs) + - * /)
          (xitomatl AS-match)
          (mpl sum-product-power))

  (define (simplify-quotient u)
    (match u
      ( ('/ x y)
        (* x (^ y -1)) )))

  (define (/ u v)
    (simplify-quotient `(/ ,u ,v)))

  )