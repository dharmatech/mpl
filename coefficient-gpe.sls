#!r6rs

(library (mpl coefficient-gpe)

  (export coefficient-gpe coefficient-monomial-gpe)

  (import (mpl rnrs-sans)
          (mpl misc)
          (mpl arithmetic)
          (mpl contains))

  ;; (define (base u)
  ;;   (list-ref u 1))

  ;; (define (exponent u)
  ;;   (list-ref u 2))

  (define undefined 'undefined)

  (define (coefficient-monomial-gpe u x)

    (cond ( (equal? u x) '(1 1) )

          ( (and (power? u)
                 (equal? (base u) x)
                 (integer? (exponent u))
                 (> (exponent u) 1))

            (list 1 (exponent u)) )

          ( (product? u)

            (let loop ( (m 0)
                        (c u)
                        (i 1) )

              (if (>= i (length u))
                  
                  (list c m)

                  (let ((f (coefficient-monomial-gpe (list-ref u i) x)))

                    (cond ( (eq? f undefined) undefined )

                          ( (not (equal? (list-ref f 1) 0))

                            (let ((m (list-ref f 1)))

                              (let ((c (/ u (^ x m))))

                                (loop m c (+ i 1)))) )

                          ( else (loop m c (+ i 1)) ))))) )

          ( (free? u x) (list u 0) )

          ( else undefined )))

  (define (coefficient-gpe u x j)

    (cond ( (not (sum? u))

            (let ((f (coefficient-monomial-gpe u x)))

              (cond ( (eq? f undefined) undefined )

                    ( (equal? j (list-ref f 1)) (list-ref f 0) )

                    ( else 0 ))) )

          ( (equal? u x) (if (equal? j 1) 1 0) )

          ( else

            (let ((n (length u)))

              (let loop ( (c 0)
                          (i 1) )

                (if (>= i n)

                    c
                    
                    (let ((f (coefficient-monomial-gpe (list-ref u i) x)))

                      (cond ( (equal? f undefined) undefined )

                            ( (equal? (list-ref f 1) j)

                              (loop (+ c (list-ref f 0))
                                    (+ i 1)) )

                            ( else (loop c (+ i 1)) )))))) )))

  )