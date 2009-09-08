
(library (mpl leading-coefficient-gpe)

  (export leading-coefficient-gpe)

  (import (rnrs)
          (mpl degree-gpe)
          (mpl coefficient-gpe))

  (define (leading-coefficient-gpe u x)
    (coefficient-gpe u x (degree-gpe u (list x)))))
