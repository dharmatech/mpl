
(library (mpl cos)

  (export cos)

  (import (except (rnrs) + - * / sin cos exp)
          (xitomatl AS-match)
          (mpl automatic-simplification))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define pi 'pi)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (simplify-cos-first-quadrant a/b)

    (cond ( (> a/b 2)   (cos (* (mod a/b 2) pi))    )

          ( (> a/b 1)   (- (cos (- (* a/b pi) pi))) )

          ( (> a/b 1/2) (- (cos (- pi (* a/b pi)))) )

          ( else       `(cos ,(* a/b pi))           )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (simplify-cos-k/n*pi k/n)

    (let ((k (numerator   k/n))
          (n (denominator k/n)))

      (case n

        ((1) (case (mod k 2)
               ((1) -1)
               ((0)  1)))

        ((2) (case (mod k 2)
               ((1) 0)))

        ((3) (case (mod k 6)
               ((1 5)  1/2)
               ((2 4) -1/2)))

        ((4) (case (mod k 8)
               ((1 7)    (/ 1 '(sqrt 2)))
               ((3 5) (- (/ 1 '(sqrt 2))))))

        ((6) (case (mod k 12)
               ((1 11)    (/ '(sqrt 3) 2))
               ((5 7)  (- (/ '(sqrt 3) 2))))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (simplify-cos u)

    (match u

      ( ('cos 0) 1 )

      ( ('cos 'pi) -1 )

      ( (and ('cos n)
             (? (lambda (_)
                  (and (number? n)
                       (negative? n)))))
        (cos (- n)) )

      ( (and ('cos ('* n . elts))
             (? (lambda (_)
                  (and (number? n)
                       (negative? n)))))
        (cos (apply * (append (list -1 n) elts))) )

      ( (and ('cos ('* a/b 'pi))
             (? (lambda (_)
                  (and (exact? a/b)
                       (> a/b 1/2)))))
        (simplify-cos-first-quadrant a/b) )

      ( (and ('cos ('* k/n 'pi))
             (? (lambda (_)
                  (and (member (denominator k/n) '(1 2 3 4 6))
                       (integer? (numerator k/n))))))

        (simplify-cos-k/n*pi k/n) )

      ( else u )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (cos x)
    (simplify-cos `(cos ,x)))

  )