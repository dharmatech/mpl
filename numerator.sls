
(library (mpl numerator)

  (export numerator)

  (import (except (rename (rnrs) (numerator rnrs:numerator)) + - * / exp)
          (xitomatl AS-match)
          (mpl automatic-simplification))

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


          