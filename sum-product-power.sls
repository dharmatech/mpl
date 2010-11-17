#!r6rs

(library (mpl sum-product-power)

  (export + * ^
          simplify-sum
          simplify-product
          simplify-power)

  (import (rename (rnrs) (+ rnrs:+) (* rnrs:*))
          (mpl util match)
          (mpl util equivalence)
          (mpl util list)
          (mpl order-relation))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (list-or-null-if-0 x)
    (if (equal? x 0)
        '()
        (list x)))
  
  (define (list-or-null-if-1 x)
    (if (equal? x 1)
        '()
        (list x)))

  (define any-are-zero? (any-are (equal-to 0)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ^
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (raise-to expo)
    (lambda (base)
      (^ base expo)))

  (define (^ v w)
    (match (list v w)
      ((0 w) 0)
      ((1 w) 1)
      ((v 0) 1)
      ((v 1) v)
      (((? number?) (? integer?)) (expt v w))
      ((('^ r s)    (? integer?)) (^ r (* s w)))
      ((('* . vs)   (? integer?)) (apply * (map (raise-to w) vs)))
      (else `(^ ,v ,w) )))

  (define (simplify-power u)
    (^ (list-ref u 1)
       (list-ref u 2)))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; *
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (merge-products p-elts q-elts)
    (match (list p-elts q-elts)
      ( (() x) x )
      ( (x ()) x )
      ( ((p . ps) (q . qs))
        (match (simplify-product-rec (list p q))
          ( ()                                (merge-products ps qs)      )
          ( (x)                       (cons x (merge-products ps qs))     )
          ( (? (equal-to (list p q))) (cons p (merge-products ps q-elts)) )
          ( (? (equal-to (list q p))) (cons q (merge-products p-elts qs)) )) )))

  (define (simplify-product-rec elts)
    (match elts
      ( (('* . p-elts) ('* . q-elts)) (merge-products p-elts   q-elts)   )
      ( (('* . p-elts) q)             (merge-products p-elts   (list q)) )
      ( (p             ('* . q-elts)) (merge-products (list p) q-elts)   )
      ( ((? number? p) (? number? q)) (list-or-null-if-1 (rnrs:* p q))   )
      ( (1 x) (list x) )
      ( (x 1) (list x) )
      ( (p q) (cond ((equal? (base p) (base q))
                     (list-or-null-if-1
                      (^ (base p)
                         (+ (exponent p)
                            (exponent q)))))

                    ((order-relation q p) (list q p))

                    (else (list p q))) )
      ( (('* . ps) . qs) (merge-products ps       (simplify-product-rec qs)) )
      ( (x         . xs) (merge-products (list x) (simplify-product-rec xs)) )))

  (define (simplify-product u)
    (match u
      ( ('* x) x )
      ( ('* . (? any-are-zero?)) 0 )
      ( ('* . elts)
        (match (simplify-product-rec elts)
          ( () 1 )
          ( (x) x )
          ( xs `(* ,@xs) )) )))

  (define (* . elts)
    (simplify-product `(* ,@elts)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; +
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (merge-sums p-elts q-elts)
    (match (list p-elts q-elts)
      ( (() x) x )
      ( (x ()) x )
      ( ((p . ps) (q . qs))
        (match (simplify-sum-rec (list p q))
          ( ()                                (merge-sums ps qs)      )
          ( (x)                       (cons x (merge-sums ps qs))     )
          ( (? (equal-to (list p q))) (cons p (merge-sums ps q-elts)) )
          ( (? (equal-to (list q p))) (cons q (merge-sums p-elts qs)) )) )))

  (define (simplify-sum-rec elts)
    (match elts
      ( (('+ . p-elts) ('+ . q-elts)) (merge-sums p-elts q-elts)       )
      ( (('+ . p-elts) q)             (merge-sums p-elts (list q))     )
      ( (p ('+ . q-elts))             (merge-sums (list p) q-elts)     )
      ( ((? number? p) (? number? q)) (list-or-null-if-0 (rnrs:+ p q)) )
      ( (0 x) (list x) )
      ( (x 0) (list x) )
      ( (p q) (cond ((equal? (term p) (term q))
                     (list-or-null-if-0
                      (* (term p)
                         (+ (const p)
                            (const q)))))

                    ((order-relation q p)
                     (list q p))

                    (else (list p q))) )
      ( (('+ . ps) . qs) (merge-sums ps       (simplify-sum-rec qs)) )
      ( (x         . xs) (merge-sums (list x) (simplify-sum-rec xs)) )))

  (define (simplify-sum u)
    (match u
      ( ('+ x) x )
      ( ('+ . elts)
        (match (simplify-sum-rec elts)
          ( () 0 )
          ( (x) x )
          ( xs `(+ ,@xs) )) )))

  (define (+ . elts)
    (simplify-sum `(+ ,@elts)))

  )