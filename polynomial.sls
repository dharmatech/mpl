
(library (mpl polynomial) 

  (export polynomial?
          polynomial-in?
          is-polynomial?)

  (import (rnrs)
          (only (srfi :1) every)
          (mpl misc)
          (mpl monomial))
  
  (define (polynomial? u v)
    (or (monomial? u v)
        (and (sum? u)
             (every (monomial-in? v) (cdr u)))))

  (define (polynomial-in? v)
    (lambda (u)
      (polynomial? u v)))

  (define (is-polynomial? u)
    (lambda (v)
      (polynomial? u v)))

  )