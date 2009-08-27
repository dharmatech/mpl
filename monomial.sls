
(library (mpl monomial)

  (export monomial?
          monomial-in?
          is-monomial?)

  (import (rnrs)
          (only (srfi :1) every)
          (mpl misc)
          (mpl contains)
          )

  (define (monomial? u v)

    (or (every (is-free? u) v) ;; GME-1

        (member u v) ;; GME-2

        (and (power? u) ;; GME-3
             (let ((n (list-ref u 2)))
               (integer? n)
               (> n 1))) 

        (and (product? u) ;; GME-4
             (every (monomial-in? v) (cdr u))))) 

  (define (monomial-in? v)
    (lambda (u)
      (monomial? u v)))

  (define (is-monomial? u)
    (lambda (v)
      (monomial? u v)))

  )


  