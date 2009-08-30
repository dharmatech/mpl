
(library (mpl automatic-simplification) 

  (export simplify-power
          simplify-product
          simplify-sum
          simplify-quotient
          simplify-difference

          ^ * + / - !


          simplify-sum-rec
          merge-sums
          
          simplify-factorial)

  (import (rename (rnrs)
                  (+ rnrs:+)
                  (- rnrs:-)
                  (* rnrs:*)
                  (/ rnrs:/))

          (only (srfi :1) any)
          
          (xitomatl AS-match)
          (mpl misc)
          (mpl order-relation))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (simplify-integer-power v n)

    (cond ((= n 0) 1) ;; SINTPOW-2
          ((= n 1) v) ;; SINTPOW-3

          (else
          
           (match v

             ( (? number?) (expt v n) ) ;; SINTPOW-1

             ( ('^ r s) ;; SINTPOW-4
               (let ((p (* s n)))
                 (^ r p)) )

             ( ('* . elts) ;; SINTPOW-5
               (apply *
                      (map (lambda (elt)
                             (^ elt n))
                           elts)) )

             ( else `(^ ,v ,n) )))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (simplify-power u)

    (match u

      ( ('^ 0 (? number? w)) (expt u w) ) ;; SPOW-2

      ( ('^ 1 w) 1 ) ;; SPOW-3

      ( ('^ v (? integer? w)) (simplify-integer-power v w) ) ;; SPOW-4

      ( else u )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (^ v w)
    (simplify-power `(^ ,v ,w)))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (merge-products p q)

    (cond ((null? q) p) ;; MPRD-1

          ((null? p) q) ;; MPRD-2

          (else 

           (let ((p1 (car p))
                 (q1 (car q)))

             (match (simplify-product-rec (list p1 q1))

               ( () (merge-products (cdr p) (cdr q)) ) ;; MPRD-3-1

               ( (h1) (cons h1 (merge-products (cdr p) (cdr q))) ) ;; MPRD-3-2

               ( (and h
                      (? (lambda (_)
                           (equal? h (list p1 q1)))))

                 (cons p1 (merge-products (cdr p) q)) )

               ( (and h
                      (? (lambda (_)
                           (equal? h (list q1 p1)))))

                 (cons q1 (merge-products p (cdr q))) ))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (simplify-product-rec elts)

    (match elts

      ( (('* . p-elts) ('* . q-elts)) ;; SPRDREC-2-1

        (merge-products p-elts q-elts) )

      ( (('* . p-elts) u2) ;; SPRDREC-2-2

        (merge-products p-elts (list u2)) )

      ( (u1 ('* . q-elts)) ;; SPRDREC-2-3

        (merge-products (list u1) q-elts) )

      ( ((? number? u1) (? number? u2)) ;; SPRDREC-1-1

        (let ((P (rnrs:* u1 u2)))

          (if (= P 1)
              '()
              (list P))) )

      ( (and (u1 u2) ;; SPRDREC-1-2-a

             (? (lambda (_)

                  (equal? u1 1))))

        (list u2)  )

      ( (and (u1 u2) ;; SPRDREC-1-2-b

             (? (lambda (_)

                  (equal? u2 1))))

        (list u1)  )

      ( (and (u1 u2) ;; SPRDREC-1-3

             (? (lambda (_)

                  (equal? (base u1)
                          (base u2)))))

        (let ((S (+ (exponent u1)
                    (exponent u2))))

          (let ((P (^ (base u1) S)))

            (if (equal? P 1)
                '()
                (list P)))) )

      ( (and (u1 u2) ;; SPRDREC-1-4
             
             (? (lambda (_)

                  (order-relation u2 u1))))

        (list u2 u1) )

      ( (u1 u2) elts ) ;; SPRDREC-1-5

      ( (and elts
             (? (lambda (_)
                  (> (length elts) 2)))) ;; SPRDREC-3

        (let ((w (simplify-product-rec (cdr elts))))

          (if (product? (car elts))

              (merge-products (cdr (car elts)) w)

              (merge-products (list (car elts)) w))) )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (simplify-product u)

    (match u

      ;; SPRD-1

      ( ('* elt) elt ) ;; SPRD-3

      ( (and ('* . elts) ;; SPRD-2

             (? (lambda (_)

                  (any (lambda (elt)
                         (equal? elt 0))

                       elts))))

        0 )

      ( (and ('* . elts) ;; SPRD-2

             (? (lambda (_)

                  (any (lambda (elt)
                         (equal? elt 0.0))

                       elts))))

        0.0 )

      ( ('* . elts) ;; SPRD-4

        (let ((v (simplify-product-rec elts)))

          (match v

            ( () 1 ) ;; SPRD-4-3

            ( (elt) elt ) ;; SPRD-4-1

            ( elts `(* ,@elts) ) ;; SPRD-4-2

            )) )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (* . elts)

    (simplify-product `(* ,@elts)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (merge-sums p q)

    (cond ((null? q) p) ;; MPRD-1

          ((null? p) q) ;; MPRD-2

          (else 

           (let ((p1 (car p))
                 (q1 (car q)))

             (match (simplify-sum-rec (list p1 q1))

               ( () (merge-sums (cdr p) (cdr q)) ) ;; MPRD-3-1

               ( (h1) (cons h1 (merge-sums (cdr p) (cdr q))) ) ;; MPRD-3-2

               ( (and h
                      (? (lambda (_)
                           (equal? h (list p1 q1)))))

                 (cons p1 (merge-sums (cdr p) q)) )

               ( (and h
                      (? (lambda (_)
                           (equal? h (list q1 p1)))))

                 (cons q1 (merge-sums p (cdr q))) ))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (simplify-sum-rec elts)

    (match elts

      ( (('+ . p-elts) ('+ . q-elts)) ;; SPRDREC-2-1

        (merge-sums p-elts q-elts) )

      ( (('+ . p-elts) u2) ;; SPRDREC-2-2

        (merge-sums p-elts (list u2)) )

      ( (u1 ('+ . q-elts)) ;; SPRDREC-2-3

        (merge-sums (list u1) q-elts) )

      ( ((? number? u1) (? number? u2)) ;; SPRDREC-1-1

        (let ((P (rnrs:+ u1 u2)))

          (if (= P 0)
              '()
              (list P))) )

      ( (0 u2) (list u2) ) ;; SPRDREC-1-2-a

      ( (u1 0) (list u1) ) ;; SPRDREC-1-2-b

      ( (and (u1 u2) ;; SPRDREC-1-3

             (? (lambda (_)

                  (equal? (term u1)
                          (term u2)))))

        (let ((S (+ (const u1)
                    (const u2))))

          (let ((P (* (term u1) S)))

            (if (equal? P 0)
                '()
                (list P)))) )

      ( (and (u1 u2) ;; SPRDREC-1-4
             
             (? (lambda (_)

                  (order-relation u2 u1))))

        (list u2 u1) )

      ( (u1 u2) elts ) ;; SPRDREC-1-5

      ( (and elts
             (? (lambda (_)
                  (> (length elts) 2)))) ;; SPRDREC-3

        (let ((w (simplify-sum-rec (cdr elts))))

          (if (sum? (car elts))

              (merge-sums (cdr (car elts)) w)

              (merge-sums (list (car elts)) w))) )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (simplify-sum u)

    (match u

      ;; SPRD-1

      ( ('+ elt) elt ) ;; SPRD-3

      ;; ( (and ('+ . elts) ;; SPRD-2

      ;;        (? (lambda (_)

      ;;             (any (lambda (elt)
      ;;                    (equal? elt 0))

      ;;                  elts))))

      ;;   0 )

      ;; ( (and ('* . elts) ;; SPRD-2

      ;;        (? (lambda (_)

      ;;             (any (lambda (elt)
      ;;                    (equal? elt 0.0))

      ;;                  elts))))

      ;;   0.0 )

      ( ('+ . elts) ;; SPRD-4

        (let ((v (simplify-sum-rec elts)))

          (match v

            ( () 0 ) ;; SPRD-4-3

            ( (elt) elt ) ;; SPRD-4-1

            ( elts `(+ ,@elts) ) ;; SPRD-4-2

            )) )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (+ . elts)
    (simplify-sum `(+ ,@elts)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (simplify-quotient u)
    (match u
      ( ('/ x y)
        (* x (^ y -1)) )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (/ u v)
    (simplify-quotient `(/ ,u ,v)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (simplify-difference u)

    (match u

      ( ('- x) (* -1 x) )

      ( ('- x y) (+ x (* -1 y)) )))

  (define (- . elts)
    (simplify-difference `(- ,@elts)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (factorial n)
    (if (= n 1)
        1
        (rnrs:* n (factorial (rnrs:- n 1)))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (simplify-factorial u)
    (match u
      ( ('! (? number? n)) (factorial n) )
      ( ('! n) u )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (! n)
    (simplify-factorial `(! ,n)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  )