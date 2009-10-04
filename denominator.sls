
(library (mpl denominator)

  (export denominator)

  (import (mpl rnrs-sans)
          (rename (only (rnrs) denominator) (denominator rnrs:denominator))
          (xitomatl AS-match)
          (mpl arithmetic))

  (define (denominator u)

    (match u

      ( (? number?) (rnrs:denominator u) )

      ( ('^ x y)
        (if (and (number? y)
                 (negative? y))
            (^ u -1)
            1) )

      ( ('* v . rest)
        (* (denominator v)
           (denominator (/ u v))) )

      ( else 1 )))

  )
