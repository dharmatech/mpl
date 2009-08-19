
(library (mpl test) 

  (export test) 

  (import (except (rnrs) + - * /)
          (srfi :64)
          (numero symbolic alg)
          (mpl misc)
          (mpl automatic-simplification)
          (mpl automatic-simplify)
          (mpl substitute)
          )

  (define (test)

    (vars a b c d x y z)

    (test-begin "mpl")

    (test-equal "Figure 1.5"
                (- (/ (* x y) 3))
                '(* -1/3 x y)
                )

    (test-equal "Example 3.35"
                (^ (^ (^ x 1/2) 1/2) 8)
                '(^ x 2)
                )

    (test-equal "Example 3.36"
                (^ (* (^ (* x y) 1/2) (^ z 2)) 2)
                '(* x y (^ z 4))
                )

    (test-equal "3.2 Exercise 3-a"
                (/ x x)
                1
                )

    (test-equal "3.2 Exercise 3-b"
                (* (/ x y)
                   (/ y x))
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

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; substitute
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-equal "Figure 3.23 - 2"
                (substitute (alg "a+b") b x)
                (+ a x)
                )

    (test-equal "Figure 3.23 - 4"
                (substitute (alg "1/a+a") a x)
                (+ (^ x -1) x)
                )

    (test-equal "Figure 3.23 - 5"
                (substitute (alg "(a+b)^2 + 1") (alg "a+b") x)
                (+ 1 (^ x 2))
                )

    (test-equal "Figure 3.23 - 6"
                (substitute '(+ a b c) '(+ a b) x)
                (+ a b c)
                )

    (test-equal "Figure 3.23 - 7"
                (substitute (+ a b c) a (alg "x-b"))
                (+ c x)
                )

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; sequential-substitute
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-equal "EA: Example 3.32 - 1"
                (sequential-substitute (alg "x+y")
                                       `( ( x ,(alg "a+1") )
                                          ( y ,(alg "b+2") )))
                (+ 3 a b)
                )

    (test-equal "EA: Example 3.32 - 2"
                (sequential-substitute (alg "x+y")
                                       `( ( x ,(alg "a+1") )
                                          ( a ,(alg "b+2") )))
                (+ 3 b y)
                )

    (test-equal "EA: Example 3.32 - 3"
                (sequential-substitute (alg "f(x)=a*x+b")
                                       '( ( (f x) 2 )
                                          ( x     3 ) ))
                '(= 2 (+ (* 3 a) b))
                )

    (test-equal "EA: Example 3.32 - 4"
                (sequential-substitute (alg "f(x)=a*x+b")
                                       '( ( x 3 )
                                          ( (f x) 2 ) ))
                '(= (f 3) (+ (* 3 a) b))
                )

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-equal "EA: Example 3.35 - 1"
                (concurrent-substitute (alg "(a+b)*x")
                                       `( ( ,(alg "a+b") ,(alg "x+c") )
                                          ( x d ) ))
                (* d (+ c x))
                )

    (test-equal "EA: Example 3.35 - 2"
                (concurrent-substitute (alg "f(x)=a*x+b")
                                       '( ( x 3 )
                                          ( (f x) 2 ) ))
                '(= 2 (+ (* 3 a) b))
                )
     
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-end "mpl")

    )

  )
  