
(library (mpl polynomial-gpe) 

  (export polynomial-gpe?
          polynomial-gpe-in?
          is-polynomial-gpe?)

  (import (rnrs)
          (only (surfage s1 lists) every)
          (mpl misc)
          (mpl monomial-gpe))
  
  (define (polynomial-gpe? u v)
    (or (monomial-gpe? u v)
        (and (sum? u)
             (every (monomial-gpe-in? v) (cdr u)))))

  (define (polynomial-gpe-in? v)
    (lambda (u)
      (polynomial-gpe? u v)))

  (define (is-polynomial-gpe? u)
    (lambda (v)
      (polynomial-gpe? u v)))

  )