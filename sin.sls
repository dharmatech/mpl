
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

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (n*pi? elt)
    (match elt
      ( (and ('* n 'pi)
             (? (lambda (_)
                  (and (exact? n)
                       (>= (abs n) 2)))))
        #t )
      ( else #f )))

  (define (simplify-sum-with-pi elts)

    (let ((pi-elt (find n*pi? elts)))

      (let ((n (list-ref pi-elt 1)))

        (sin (+ (- (apply + elts) pi-elt)
                (* (mod n 2) pi))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (n/2*pi? elt)
    (define (n/2? x)
      (and (exact? x)
           (equal? (denominator x) 2)))
    (match elt
      ( ('* (? n/2?) 'pi) #t )
      ( else              #f )))

  (define (simplify-sin-sum-with-n/2*pi elts)
    (let ((n/2*pi (find n/2*pi? elts)))
      (let ((other-elts (- (apply + elts) n/2*pi)))
        (let ((n (numerator (list-ref n/2*pi 1))))
          (case (mod n 4)
            ((1) `(cos ,other-elts))
            ((3) (- `(cos ,other-elts))))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

      ( (and ('sin ('+ . elts))
             (? (lambda (_)
                  (find n*pi? elts))))
        (simplify-sum-with-pi elts) )

      ( (and ('sin ('+ . elts))
             (? (lambda (_)
                  (find n/2*pi? elts))))
        (simplify-sin-sum-with-n/2*pi elts) )

      ( else u )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (sin x)
    (simplify-sin `(sin ,x)))

  )