
(library (mpl simplify-trig)

  (export simplify-trig)

  (import (except (rnrs) + - * / sin cos exp numerator denominator)
          (mpl automatic-simplification)
          (mpl numerator)
          (mpl denominator)
          (mpl rationalize-expression)
          (mpl expand-trig)
          (mpl contract-trig)
          (mpl trig-substitute))

  (define (simplify-trig u)
    (let ((w (rationalize-expression (trig-substitute u))))
      (/ (contract-trig (expand-trig (numerator   w)))
         (contract-trig (expand-trig (denominator w))))))

  )
