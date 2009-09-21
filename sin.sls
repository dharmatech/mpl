
(library (mpl sin)

  (export sin)

  (import (except (rnrs) + - * / sin cos exp)
          (xitomatl AS-match)
          (mpl automatic-simplification))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define pi 'pi)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (simplify-sin-first-quadrant a/b)

    (cond ( (> a/b 2)

            (sin (* (mod a/b 2) pi)) )

          ( (> a/b 1)

            (- (sin (- (* a/b pi) pi))) )

          ( (> a/b 1/2)

            (sin (* (- 1 a/b) pi)) )

          ( else `(sin ,(* a/b pi)) )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

      ( ('sin 0) 0 )

      ( ('sin 'pi) 0 )

      ( (and ('sin n)
             (? (lambda (_)
                  (and (number? n)
                       (negative? n)))))
        (- (sin (* -1 n))) )

      ( (and ('sin ('* n . elts))
             (? (lambda (_)
                  (and (number? n)
                       (negative? n)))))

        (- (sin (apply * (append (list -1 n) elts)))) )

      ( (and ('sin ('* a/b 'pi))
             (? (lambda (_)
                  (and (exact? a/b)
                       (> a/b 1/2)))))
        (simplify-sin-first-quadrant a/b) )

      ( (and ('sin ('* k/n 'pi))
             (? (lambda (_)
                  (and (member (denominator k/n) '(1 2 3 4 6))
                       (integer? (numerator k/n))))))

        (simplify-sin-k/n*pi k/n) )

      ( else u )))

  (define (sin x)
    (simplify-sin `(sin ,x)))

  )