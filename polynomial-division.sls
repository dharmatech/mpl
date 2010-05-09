
(library (mpl polynomial-division)

  (export polynomial-division)

  (import (mpl rnrs-sans)
          (mpl arithmetic)
          (mpl util)
          (mpl degree-gpe)
          (mpl algebraic-expand)
          (mpl leading-coefficient-gpe))

  (define (polynomial-division u v x)

    (let* ((q 0)
           (r u)
           (m (degree-gpe r (list x)))
           (n (degree-gpe v (list x)))
           (lcv (leading-coefficient-gpe v x)))

      (while (>= m n)

        (let* ((lcr (leading-coefficient-gpe r x))
               (s (/ lcr lcv)))

          (set! q (+ q (* s (^ x (- m n)))))

          (set! r (algebraic-expand (- (- r (* lcr (^ x m)))
                                       (* (- v (* lcv (^ x n)))
                                          s
                                          (^ x (- m n))))))

          (set! m (degree-gpe r (list x)))))

      (list q r))))