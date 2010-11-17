#!r6rs

(library (mpl variables) 

  (export variables) 

  (import (rnrs)
          (mpl misc)
          (only (surfage s1 lists) lset-union)
          )

  (define (union . lists)
    (apply lset-union (cons equal? lists)))

  (define (VAR-1 u)
    (and (number? u) '()))

  (define (VAR-2 u)
    (and (power? u) ;; VAR-2
         (let ((n (list-ref u 2)))
           (and (integer? n)
                (> n 1)))
         (list (list-ref u 1))))

  (define (VAR-3 u)
    (and (sum? u)
         (apply union
                (map
                 (lambda (operand)
                   (or (VAR-1 operand)
                       (VAR-2 operand)
                       (VAR-4 operand)
                       (VAR-5 operand)))
                 (cdr u)))))

  (define (VAR-4 u)
    (and (product? u)
         (apply union
                (map 
                 (lambda (operand)
                   (or (VAR-1 operand)
                       (VAR-2 operand)
                       (and (sum? operand) (list operand))
                       (VAR-5 operand)))
                 (cdr u)))))

  (define (VAR-5 u)
    (list u))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (variables u)
    (or (VAR-1 u)
        (VAR-2 u)
        (VAR-3 u)
        (VAR-4 u)
        (VAR-5 u)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  )