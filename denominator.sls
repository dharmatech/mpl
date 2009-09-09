
(library (mpl denominator)

  (export denominator)

  (import (except (rename (rnrs) (denominator rnrs:denominator)) + - * / exp)
          (xitomatl AS-match)
          (mpl automatic-simplification))

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
