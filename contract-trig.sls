
(library (mpl contract-trig)

  (export contract-trig
          contract-trig-rules
          contract-trig-power
          contract-trig-product
          )

  (import (except (rnrs) + - * / sin cos exp)
          (mpl misc)
          (mpl automatic-simplification)
          (mpl sin)
          (mpl cos)
          (mpl separate-sin-cos)
          (mpl expand-main-op))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (binomial-coefficient n k)
    (cond ( (= k 0) 1 )
          ( (= n k) 1 )
          ( else
            (+ (binomial-coefficient (- n 1)
                                     (- k 1))
               (binomial-coefficient (- n 1)
                                     k)) )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (sigma f a b step)
    (let loop ((a a) (sum 0))
      (if (> a b)
          sum
          (loop (+ a step)
                (+ sum (f a))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; (define (contract-trig-power u)

  ;;   (cond ( (and (cos?      (base u))
  ;;                (integer?  (exponent u))
  ;;                (positive? (exponent u))
  ;;                (even?     (exponent u)))

  ;;           (let ((x (list-ref (base u) 1))
  ;;                 (n (exponent u)))

  ;;             (+ (/ (binomial-coefficient n (/ n 2))
  ;;                   (^ 2 n))

  ;;                (* (/ 1 (^ 2 (- n 1)))

  ;;                   (sigma (lambda (j)
  ;;                            (* (binomial-coefficient n j)
  ;;                               (cos (* (- n (* 2 j)) x))))
  ;;                          0
  ;;                          (- (/ n 2) 1)
  ;;                          1)))) )

  ;;         ( (and (cos?      (base u))
  ;;                (integer?  (exponent u))
  ;;                (positive? (exponent u))
  ;;                (odd?      (exponent u)))

  ;;           (let ((x (list-ref (base u) 1))
  ;;                 (n (exponent u)))

  ;;             (* (/ 1 (^ 2 (- n 1)))

  ;;                (sigma (lambda (j)
  ;;                         (* (binomial-coefficient n j)
  ;;                            (cos (* (- n (* 2 j)) x))))
  ;;                       0
  ;;                       (floor (/ n 2))
  ;;                       1))) )

  ;;         ( (and (sin?      (base u))
  ;;                (integer?  (exponent u))
  ;;                (positive? (exponent u))
  ;;                (even?     (exponent u)))

  ;;           (let ((x (list-ref (base u) 1))
  ;;                 (n (exponent u)))

  ;;             (+ (/ (* (^ -1 n)
  ;;                      (binomial-coefficient n (/ n 2)))
  ;;                   (^ 2 n))

  ;;                (* (/ (^ -1 (/ n 2))
  ;;                      (^ 2 (- n 1)))

  ;;                   (sigma (lambda (j)
  ;;                            (* (^ -1 j)
  ;;                               (binomial-coefficient n j)
  ;;                               (cos (* (- n (* 2 j)) x))))
  ;;                          0
  ;;                          (- (/ n 2) 1)
  ;;                          1)))) )

  ;;         ( (and (sin?      (base u))
  ;;                (integer?  (exponent u))
  ;;                (positive? (exponent u))
  ;;                (odd?      (exponent u)))

  ;;           (let ((x (list-ref (base u) 1))
  ;;                 (n (exponent u)))

  ;;             (* (/ (^ -1 (/ (- n 1) 2))
  ;;                   (^ 2 (- n 1)))

  ;;                (sigma (lambda (j)
  ;;                         (* (binomial-coefficient n j)
  ;;                            (^ -1 j)
  ;;                            (sin (* (- n (* 2 j)) x))))
  ;;                       0
  ;;                       (floor (/ n 2))))) )

  ;;         (else u)

  ;;         ))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (contract-trig-power u)

    (cond ( (and (cos?      (base u))
                 (integer?  (exponent u))
                 (positive? (exponent u))
                 (even?     (exponent u)))

            (let ((x (list-ref (base u) 1))
                  (n (exponent u)))

              (contract-trig-rules

               (+ (/ (binomial-coefficient n (/ n 2))
                     (^ 2 n))

                  (* (/ 1 (^ 2 (- n 1)))

                     (sigma (lambda (j)
                              (* (binomial-coefficient n j)
                                 (cos (* (- n (* 2 j)) x))))
                            0
                            (- (/ n 2) 1)
                            1))))) )

          ( (and (cos?      (base u))
                 (integer?  (exponent u))
                 (positive? (exponent u))
                 (odd?      (exponent u)))

            (let ((x (list-ref (base u) 1))
                  (n (exponent u)))

              (contract-trig-rules

               (* (/ 1 (^ 2 (- n 1)))

                  (sigma (lambda (j)
                           (* (binomial-coefficient n j)
                              (cos (* (- n (* 2 j)) x))))
                         0
                         (floor (/ n 2))
                         1)))) )

          ( (and (sin?      (base u))
                 (integer?  (exponent u))
                 (positive? (exponent u))
                 (even?     (exponent u)))

            (let ((x (list-ref (base u) 1))
                  (n (exponent u)))

              (contract-trig-rules
               
               (+ (/ (* (^ -1 n)
                        (binomial-coefficient n (/ n 2)))
                     (^ 2 n))

                  (* (/ (^ -1 (/ n 2))
                        (^ 2 (- n 1)))

                     (sigma (lambda (j)
                              (* (^ -1 j)
                                 (binomial-coefficient n j)
                                 (cos (* (- n (* 2 j)) x))))
                            0
                            (- (/ n 2) 1)
                            1))))) )

          ( (and (sin?      (base u))
                 (integer?  (exponent u))
                 (positive? (exponent u))
                 (odd?      (exponent u)))

            (let ((x (list-ref (base u) 1))
                  (n (exponent u)))

              (contract-trig-rules

               (* (/ (^ -1 (/ (- n 1) 2))
                     (^ 2 (- n 1)))

                 (sigma (lambda (j)
                          (* (binomial-coefficient n j)
                             (^ -1 j)
                             (sin (* (- n (* 2 j)) x))))
                        0
                        (floor (/ n 2)))))) )

          (else u)

          ))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (contract-trig-product u)

    (if (= (length (cdr u)) 2)

        (let ((A (list-ref u 1))
              (B (list-ref u 2)))

          (cond ( (power? A)
                  (let ((A (contract-trig-power A)))
                    (contract-trig-rules (* A B))) )

                ( (power? B)
                  (let ((B (contract-trig-power B)))
                    (contract-trig-rules (* A B))) )

                ( else

                  (let ((x (list-ref A 1))
                        (y (list-ref B 1)))

                    (cond ( (and (sin? A) (sin? B))

                            (- (/ (cos (- x y)) 2)
                               (/ (cos (+ x y)) 2)) )

                          ( (and (cos? A) (cos? B))

                            (+ (/ (cos (+ x y)) 2)
                               (/ (cos (- x y)) 2)) )

                          ( (and (sin? A) (cos? B))

                            (+ (/ (sin (+ x y)) 2)
                               (/ (sin (- x y)) 2)) )

                          ( (and (cos? A) (sin? B))

                            (+ (/ (sin (+ x y)) 2)
                               (/ (sin (- y x)) 2)) ))) )))

        (let ((A (list-ref u 1)))
          (let ((B (contract-trig-product (/ u A))))
            (contract-trig-rules (* A B))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (contract-trig-rules u)

    (let ((v (expand-main-op u)))

      (cond ( (power? v)
              (contract-trig-power v) )

            ( (product? v)

              (let ((s (separate-sin-cos v)))

                (let ((c (list-ref s 0))
                      (d (list-ref s 1)))

                  (cond ( (or (equal? d 1)
                              (sin? d)
                              (cos? d))
                          v )

                        ( (power? d)

                          (expand-main-op (* c (contract-trig-power d))) )

                        ( else

                          (expand-main-op (* c (contract-trig-product d))) )))) )

            ( (sum? v)

              (let loop ( (s 0)
                          (exprs (cdr v)) )

                (if (null? exprs)
                    s
                    (let ((y (car exprs)))
                      (if (or (product? y)
                              (power?   y))
                          (loop (+ s (contract-trig-rules y))
                                (cdr exprs))
                          (loop (+ s y)
                                (cdr exprs)))))) )

            ( else v ))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (contract-trig u)
    (if (or (number? u)
            (symbol? u))
        u
        (let ((v (map contract-trig u)))
          (if (or (product? v)
                  (power? v))
              (contract-trig-rules v)
              v))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  )