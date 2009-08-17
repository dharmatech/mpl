
(library (mpl test) 

  (export test) 

  (import (except (rnrs) + - * /)
          (mpl automatic-simplification)
          (srfi :64))

  (define (test)

    (define x 'x)
    (define y 'y)
    (define z 'z)
    
    (test-begin "automatic-simplification")

    (test-equal (- (/ (* x y) 3)) ;; Figure 1.5
                '(* -1/3 x y)

    (test-equal (^ (^ (^ x 1/2) 1/2) 8) ;; Example 3.35
                '(^ x 2)
                )

    (test-equal (^ (* (^ (* x y) 1/2) (^ z 2)) 2) ;; Example 3.36
                '(* x y (^ z 4))
                )

    (test-equal (/ x x) ;; 3.2 Exercise 3-a
                1
                )

    (test-equal (* (/ x y)
                   (/ y x)) ;; 3.2 Exercise 3-b
                1
                )

    (test-equal 6
                (* 2 3)
                )

    (test-equal '(* 2 x)
                (* 2 x)
                )
    
    (test-equal '(* 2 x y z)
                (* z y x 2)
                )

    (test-equal '(^ x 5)
                (* (^ x 2) (^ x 3))
                )

    (test-equal '(+ 5 (* 2 x) y (* 2 z))
                (+ x y x z 5 z)
                )

    (test-equal '(* 1/2 x)
                (/ x 2)
                )

    (test-end "automatic-simplification")

    )

  )
  