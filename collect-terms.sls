
(library (mpl collect-terms)

  (export collect-terms)

  (import (mpl rnrs-sans)
          (only (srfi :1) iota)
          (mpl misc)
          (mpl arithmetic)
          (mpl coeff-var-monomial))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax while
    (syntax-rules ()
      ( (while test expr ...)
        (let loop ()
          (if test
              (begin expr
                     ...
                     (loop)))) )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (hashtable-values tbl)
    (call-with-values
        (lambda ()
          (hashtable-entries tbl))
      (lambda (keys vals)
        vals)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (print . elts)
    (for-each display elts))

  (define (say . elts)
    (for-each display elts)
    (newline))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (collect-terms u S)

    (cond ( (not (sum? u)) u )

          ( (member u S) u )

          ( else

            (let ((N 0)
                  (T (make-eq-hashtable)))

              (for-each
               
               (lambda (i)

                 (let ((f (coeff-var-monomial (list-ref u i) S)))

                   (let ((j 1)
                         (combined #f))

                     (while (and (not combined)
                                 (<= j N))
                       
                       (if (equal? (list-ref f 1)
                                   (list-ref (hashtable-ref T j '(#f #f)) 1))
                           (begin

                             (hashtable-set! T
                                             j
                                             (list (+ (list-ref f 0)
                                                      (list-ref (hashtable-ref T j #f)
                                                                0))
                                                   (list-ref f 1)))

                             (set! combined #t)))

                       (set! j (+ j 1)))

                     (if (not combined)
                         
                         (begin

                           (hashtable-set! T (+ N 1) f)

                           (set! N (+ N 1)))))))
               
               (cdr (iota (length u))))

              (apply +
                     (map 
                      (lambda (val)
                        (apply * val))
                      (vector->list
                       (hashtable-values T))))

              ))))

  )

  