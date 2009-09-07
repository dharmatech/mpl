
(library (mpl polynomial-gpe) 

  (export polynomial-gpe?
          polynomial-gpe-in?
          is-polynomial-gpe?)

  (import (rnrs)
          (only (srfi :1) every)
          (mpl misc)
          (mpl monomial))
  
  (define (polynomial-gpe? u v)
    (or (monomial? u v)
        (and (sum? u)
             (every (monomial-in? v) (cdr u)))))

  (define (polynomial-gpe-in? v)
    (lambda (u)
      (polynomial-gpe? u v)))

  (define (is-polynomial-gpe? u)
    (lambda (v)
      (polynomial-gpe? u v)))

  )