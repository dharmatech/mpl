
(library (mpl degree) 

  (export degree) 

  (import (rnrs)
          (only (srfi :1) every)
          (mpl misc)
          (mpl contains)
          )

  (define (degree-monomial u v)

    (cond ( (every (is-free? u) v) 0 )

          ( (member u v) 1 )

          ( (and (power? u)
                 (let ((n (list-ref u 2)))
                   (integer? n)
                   (> n 1)))

            (list-ref u 2) )

          ( (product? u)
            (apply +
                   (map
                    (lambda (elt)
                      (degree-monomial elt v))
                    (cdr u))) )

          ( else 0 )))

  (define (degree u v)

    (cond ( (sum? u)
            (apply max
                   (map
                    (lambda (elt)
                      (degree-monomial elt v))
                    (cdr u))) )

          ( else (degree-monomial u v) )))

  )