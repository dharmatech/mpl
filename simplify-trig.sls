
(library (mpl simplify-trig)

  (export simplify-trig)

  (import (except (rnrs) + - * / sin cos exp numerator denominator)
          (mpl automatic-simplification)
          (mpl numerator)
          (mpl denominator)
          (mpl rationalize-expression)
          (mpl expand-trig)
          (mpl contract-trig)
          (mpl trig-substitute)

          (mpl algebraic-expand)
          
          )

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Original version from book

  ;; (define (simplify-trig u)
  ;;   (let ((w (rationalize-expression (trig-substitute u))))
  ;;     (/ (contract-trig (expand-trig (numerator   w)))
  ;;        (contract-trig (expand-trig (denominator w))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; 2009/10/01

  ;; This version calls 'algebraic-expand' between 'contract-trig' and
  ;; 'expand-trig'. This enables 'simplify-trig' to work on EA Example 7.16.

  (define (simplify-trig u)
    (let ((w (rationalize-expression (trig-substitute u))))
      (/ (contract-trig (algebraic-expand (expand-trig (numerator   w))))
         (contract-trig (algebraic-expand (expand-trig (denominator w)))))))

  )
