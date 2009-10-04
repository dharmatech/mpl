
(library (mpl automatic-simplify)

  (export automatic-simplify)

  (import (mpl rnrs-sans)
          (mpl misc)
          (mpl sum-product-power)
          (mpl sub)
          (mpl div)
          (mpl factorial)
          (mpl exp)
          (mpl sin)
          (mpl cos)
          (mpl sqrt))

  ;; (define (automatic-simplify u)
  ;;   (if (list? u)
  ;;       (let ((v (map automatic-simplify u)))
  ;;         (cond ((power?      v) (simplify-power      v))
  ;;               ((product?    v) (simplify-product    v))
  ;;               ((sum?        v) (simplify-sum        v))
  ;;               ((quotient?   v) (simplify-quotient   v))
  ;;               ((difference? v) (simplify-difference v))
  ;;               ((factorial?  v) (simplify-factorial  v))
  ;;               (else                                 v)))
  ;;       u))

  (define (kind v)
    (and (pair? v)
         (car v)))

  (define (automatic-simplify u)
    (if (list? u)
        (let ((v (map automatic-simplify u)))
          (cond ((power?      v) (simplify-power      v))
                ((product?    v) (simplify-product    v))
                ((sum?        v) (simplify-sum        v))
                ((quotient?   v) (simplify-quotient   v))
                ((difference? v) (simplify-difference v))
                ((factorial?  v) (simplify-factorial  v))

                ( (eq? (kind v) 'exp) (apply exp (cdr v)) )
                ( (eq? (kind v) 'sin) (apply sin (cdr v)) )
                ( (eq? (kind v) 'cos) (apply cos (cdr v)) )

                ( (eq? (kind v) 'sqrt) (apply sqrt (cdr v)) )
                
                (else                                 v)))
        u))

  )