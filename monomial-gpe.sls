
(library (mpl monomial-gpe)

  (export monomial-gpe?
          monomial-gpe-in?
          is-monomial-gpe?)

  (import (rnrs)
          (only (surfage s1 lists) every)
          (mpl misc)
          (mpl contains)
          )

  (define (monomial-gpe? u v)

    (or (every (is-free? u) v) ;; GME-1

        (member u v) ;; GME-2

        (and (power? u) ;; GME-3
             (let ((n (list-ref u 2)))
               (integer? n)
               (> n 1))) 

        (and (product? u) ;; GME-4
             (every (monomial-gpe-in? v) (cdr u))))) 

  (define (monomial-gpe-in? v)
    (lambda (u)
      (monomial-gpe? u v)))

  (define (is-monomial-gpe? u)
    (lambda (v)
      (monomial-gpe? u v)))

  )


