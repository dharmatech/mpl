
(library (mpl sin)

  (export sin)

  (import (except (rnrs) + - * / sin cos exp)
          (xitomatl AS-match)
          (mpl automatic-simplification))

  (define (simplify-sin-k/n*pi k/n)

    (let ((k (numerator   k/n))
          (n (denominator k/n)))

      (case n

        ((1) 0)

        ((2) (case (mod k 4)
               ((1)  1)
               ((3) -1)))

        ((3) (case (mod k 6)
               ((1 2) (/ '(sqrt 3) 2))
               ((4 5) (- (/ '(sqrt 3) 2)))))

        ((4) (case (mod k 8)
               ((1 3) (/ 1 '(sqrt 2)))
               ((5 7) (- (/ 1 '(sqrt 2))))))

        ((6) (case (mod k 12)
               ((1 5)   1/2)
               ((7 11) -1/2))))))

  (define (simplify-sin u)

    (match u

      ( (and ('sin ('* k/n 'pi))
             (? (lambda (_)
                  (and (member (denominator k/n) '(1 2 3 4 6))
                       (integer? (numerator k/n))))))

        (simplify-sin-k/n*pi k/n) )

      ( else u )))

  (define (sin x)
    (simplify-sin `(sin ,x)))

  )