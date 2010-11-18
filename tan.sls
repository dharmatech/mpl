#!r6rs

(library (mpl tan)

  (export tan)

  (import (mpl rnrs-sans)
          (mpl match)
          (mpl arithmetic))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (simplify-tan u)

    (match u

      ( (and ('tan ('* n . elts))
             (? (lambda (_)
                  (and (number? n)
                       (negative? n)))))

        (- (tan (apply * (append (list -1 n) elts)))) )

      ( else u )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (tan x)
    (simplify-tan `(tan ,x)))

  )
