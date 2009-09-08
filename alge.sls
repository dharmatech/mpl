
(library (mpl alge)

  (export alge)

  (import (rnrs)
          (dharmalab infix alg)
          (only (mpl automatic-simplify) automatic-simplify))

  (define (alge val)
    (automatic-simplify 
     (if (string? val)
         (alg val)
         val)))

  )

  
          