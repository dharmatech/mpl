#!r6rs

(library (mpl degree-gpe) 

  (export degree-gpe) 

  (import (rnrs)
          (only (surfage s1 lists) every)
          (mpl misc)
          (mpl contains)
          )

  (define (degree-monomial-gpe u v)

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
                      (degree-monomial-gpe elt v))
                    (cdr u))) )

          ( else 0 )))

  (define (degree-gpe u v)

    (cond ( (sum? u)
            (apply max
                   (map
                    (lambda (elt)
                      (degree-monomial-gpe elt v))
                    (cdr u))) )

          ( else (degree-monomial-gpe u v) )))

  )