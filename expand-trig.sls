#!r6rs

(library (mpl expand-trig)

  (export expand-trig)

  (import (mpl rnrs-sans)
          (mpl misc)
          (mpl arithmetic)
          (mpl sin)
          (mpl cos)
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

  ;; (define (multiple-angle-sin n angle)

  ;;   (let ((f (if (sum? angle)
  ;;                (expand-trig-rules angle)
  ;;                (list (sin angle)
  ;;                      (cos angle)))))

  ;;     (let ((sin-angle (list-ref f 0))
  ;;           (cos-angle (list-ref f 1)))
        
  ;;       (sigma (lambda (j) (* (^ -1 (/ (- j 1) 2))
  ;;                             (binomial-coefficient n j)
  ;;                             (^ cos-angle (- n j))
  ;;                             (^ sin-angle j)))
  ;;              1 n 2))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (multiple-angle-sin n angle)

    (let ((f (if (sum? angle)
                 (expand-trig-rules angle)
                 (list (sin angle)
                       (cos angle)))))

      (let ((sin-angle (list-ref f 0))
            (cos-angle (list-ref f 1)))

        (let ((sign (if (< n 0) -1 1))
              (n (abs n)))

          (* sign
             (sigma (lambda (j) (* (^ -1 (/ (- j 1) 2))
                                   (binomial-coefficient n j)
                                   (^ cos-angle (- n j))
                                   (^ sin-angle j)))
                    1 n 2))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (multiple-angle-cos n angle)

    (let ((f (if (sum? angle)
                 (expand-trig-rules angle)
                 (list (sin angle)
                       (cos angle)))))

      (let ((sin-angle (list-ref f 0))
            (cos-angle (list-ref f 1)))

        (let ((n (abs n)))
        
          (sigma (lambda (j) (* (^ -1 (/ j 2))
                                (binomial-coefficient n j)
                                (^ cos-angle (- n j))
                                (^ sin-angle j)))
                 0 n 2)))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (expand-trig-rules A)

    (cond ( (sum? A)

            (let ((f (expand-trig-rules (list-ref A 1)))
                  (r (expand-trig-rules (- A (list-ref A 1)))))

              (let ((s (+ (* (list-ref f 0)
                             (list-ref r 1))
                          (* (list-ref f 1)
                             (list-ref r 0))))
                    (c (- (* (list-ref f 1)
                             (list-ref r 1))
                          (* (list-ref f 0)
                             (list-ref r 0)))))

                (list s c))) )

          ( (and (product? A)
                 (integer? (list-ref A 1)))

            (let ((f (list-ref A 1)))
              (list (multiple-angle-sin f (/ A f))
                    (multiple-angle-cos f (/ A f)))) )

          ( else (list (sin A)
                       (cos A)) )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; (define (expand-trig-rules A)

  ;;   (cond ( (sum? A)

  ;;           (let ((f (expand-trig-rules (list-ref A 1)))
  ;;                 (r (expand-trig-rules (- A (list-ref A 1)))))

  ;;             (let ((s (+ (* (list-ref f 0)
  ;;                            (list-ref r 1))
  ;;                         (* (list-ref f 1)
  ;;                            (list-ref r 0))))
  ;;                   (c (- (* (list-ref f 1)
  ;;                            (list-ref r 1))
  ;;                         (* (list-ref f 0)
  ;;                            (list-ref r 0)))))

  ;;               (list s c))) )

  ;;         ( (and (product? A)
  ;;                (integer? (list-ref A 1)))

  ;;           (let ((f (list-ref A 1)))
  ;;             (list (expand-main-op (multiple-angle-sin f (/ A f)))
  ;;                   (expand-main-op (multiple-angle-cos f (/ A f))))) )

  ;;         ( else (list (sin A)
  ;;                      (cos A)) )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; (define (expand-trig-rules A)

  ;;   (cond ( (sum? A)

  ;;           (let ((f (expand-trig-rules (list-ref A 1)))
  ;;                 (r (expand-trig-rules (- A (list-ref A 1)))))

  ;;             (let ((s (+ (expand-main-op (* (list-ref f 0)
  ;;                                            (list-ref r 1)))
  ;;                         (expand-main-op (* (list-ref f 1)
  ;;                                            (list-ref r 0)))))
  ;;                   (c (- (expand-main-op (* (list-ref f 1)
  ;;                                            (list-ref r 1)))
  ;;                         (expand-main-op (* (list-ref f 0)
  ;;                                            (list-ref r 0))))))

  ;;               (list s c))) )

  ;;         ( (and (product? A)
  ;;                (integer? (list-ref A 1)))

  ;;           (let ((f (list-ref A 1)))
  ;;             (list (expand-main-op (multiple-angle-sin f (/ A f)))
  ;;                   (expand-main-op (multiple-angle-cos f (/ A f))))) )

  ;;         ( else (list (sin A)
  ;;                      (cos A)) )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (kind u)
    (and (pair? u)
         (car u)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Original version from book

  (define (expand-trig u)

    (if (or (number? u)
            (symbol? u))

        u

        (let ((v (map expand-trig u)))

          (case (kind u)

            ( (sin) (list-ref (expand-trig-rules (list-ref v 1)) 0) )
            ( (cos) (list-ref (expand-trig-rules (list-ref v 1)) 1) )
            ( else  v )))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; (define (expand-trig u)

  ;;   (if (or (number? u)
  ;;           (symbol? u))

  ;;       u

  ;;       (let ((v (expand-main-op (map expand-trig u))))

  ;;          (case (kind u)

  ;;            ( (sin) (list-ref (expand-trig-rules (list-ref v 1)) 0) )
  ;;            ( (cos) (list-ref (expand-trig-rules (list-ref v 1)) 1) )
  ;;            ( else  v )))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  )