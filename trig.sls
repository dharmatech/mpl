
(library (mpl trig)

  (export sin cos)

  (import (rename (rnrs) (sin rnrs:sin) (cos rnrs:cos)))

  (define (sin u)
    (if (number? u)
        (rnrs:sin u)
        `(sin ,u)))

  (define (cos u)
    (if (number? u)
        (rnrs:cos u)
        `(cos ,u)))

  )
