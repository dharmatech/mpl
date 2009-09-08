
(library (mpl rational-gre)

  (export rational-gre?)

  (import (except (rnrs) numerator denominator)
          (mpl polynomial-gpe)
          (mpl numerator)
          (mpl denominator))

  (define (rational-gre? u v)
    (and (polynomial-gpe? (numerator   u) v)
         (polynomial-gpe? (denominator u) v)))

  )