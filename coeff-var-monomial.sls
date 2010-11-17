#!r6rs

(library (mpl coeff-var-monomial)

  (export coeff-var-monomial)

  (import (mpl rnrs-sans)
          (mpl arithmetic)
          (mpl contains))

  ;; (define (coeff-var-monomial u v)

  ;;   (let loop ( (coefficient-part u)
  ;;               (variables v) )

  ;;     (if (null? variables)

  ;;         (let ((variable-part (/ u coefficient-part)))

  ;;           (list coefficient-part variable-part))

  ;;         (loop (/ coefficient-part (car variables))
  ;;               (cdr variables)))))

  (define (coeff-var-monomial u v)

    (let loop ( (coefficient-part u)
                (variables v) )

      (cond ( (null? variables)

              (let ((variable-part (/ u coefficient-part)))

                (list coefficient-part variable-part)) )

            ( (free? u (car variables))

              (loop coefficient-part
                    (cdr variables)) )

            ( else

              (loop (/ coefficient-part (car variables))
                    (cdr variables)) ))))

  )