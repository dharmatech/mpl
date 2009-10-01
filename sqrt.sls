
(library (mpl sqrt)

  (export sqrt)

  (import (rename (rnrs) (sqrt rnrs:sqrt))
          (only (mpl automatic-simplification) ^))

  (define (sqrt x)
    (if (and (number? x)
             (exact? (rnrs:sqrt x)))
        (rnrs:sqrt x)
        (^ x 1/2)))

  )
          