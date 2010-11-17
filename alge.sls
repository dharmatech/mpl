#!r6rs

(library (mpl alge)

  (export alge)

  (import (rnrs)
          (mpl util infix alg)
          (mpl automatic-simplify))

  (define (alge val)
    (automatic-simplify 
     (if (string? val)
         (alg val)
         val)))

  )

  
          