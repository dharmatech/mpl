#!r6rs

(library (mpl div)

  (export / simplify-quotient)

  (import (except (rnrs) + - * /)
          (mpl util match)
          (mpl sum-product-power))

  (define (simplify-quotient u)
    (match u
      ( ('/ x y)
        (* x (^ y -1)) )))

  (define (/ u v)
    (simplify-quotient `(/ ,u ,v)))

  )