#!r6rs

(library (mpl exp)

  (export exp)

  (import (rename (rnrs) (exp rnrs:exp)))

  (define (exp u)
    (if (number? u)
        (rnrs:exp u)
        `(exp ,u)))

  )