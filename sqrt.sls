#!r6rs

(library (mpl sqrt)

  (export sqrt)

  (import (mpl rnrs-sans)
          (rename (only (rnrs) sqrt) (sqrt rnrs:sqrt))
          (mpl arithmetic))

  (define (sqrt x)
    (if (and (number? x)
             (exact? (rnrs:sqrt x)))
        (rnrs:sqrt x)
        (^ x 1/2)))

  )
