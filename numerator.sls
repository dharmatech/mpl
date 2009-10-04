
(library (mpl numerator)

  (export numerator)

  (import (mpl rnrs-sans)
          (rename (only (rnrs) numerator) (numerator rnrs:numerator))
          (xitomatl AS-match)
          (mpl arithmetic))

  (define (numerator u)

    (match u

      ( (? number?) (rnrs:numerator u) )

      ( ('^ x y)

        (if (and (number? y)
                 (negative? y))
            1
            u) )

      ( ('* v . rest)
        (* (numerator v)
           (numerator (/ u v))) )

      ( else u )))

  )


          