
(library (mpl test) 

  (export test)

  (import (except (rnrs) + - * / exp sin cos numerator denominator)
          (only (srfi :1) lset=)
          (srfi :64)
          ;; (numero symbolic alg)
          (dharmalab infix alg)
          (mpl misc)
          (mpl contains)
          (mpl automatic-simplification)
          (mpl automatic-simplify)
          (mpl alge)
          (mpl substitute)
          (mpl monomial-gpe)
          (mpl polynomial-gpe)
          (mpl variables)
          (mpl degree-gpe)
          (mpl coefficient-gpe)
          (mpl leading-coefficient-gpe)
          (mpl coeff-var-monomial)
          (mpl collect-terms)
          (mpl algebraic-expand)
          (mpl numerator)
          (mpl denominator)
          (mpl rational-gre)
          (mpl rational-variables)
          (mpl rationalize-expression)
          (mpl rational-expand)
          (mpl expand-exp)
          (mpl expand-trig)
          )

  ;; (define (alge val)
  ;;   (automatic-simplify 
  ;;    (if (string? val)
  ;;        (alg val)
  ;;        val)))

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
    ;; monomial-gpe?
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-equal "EA: Example 6.18 - 1"
                (monomial-gpe? (automatic-simplify
                            (alg "a x^2 y^2"))
                           '(x y))
                #t)

    (test-equal "EA: Example 6.18 - 2"
                (monomial-gpe? (automatic-simplify
                            (alg "x^2 + y^2"))
                           '(x y))
                #f)

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; polynomial-gpe?
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-equal "EA: Example 6.18 - 3"
                (polynomial-gpe? (automatic-simplify
                                  (alg " x^2 + y^2 "))
                                 '(x y))
                #t)

    (test-equal "EA: Example 6.18 - 4"
                (polynomial-gpe? (automatic-simplify
                                  (alg " sin(x)^2 + 2 sin(x) + 3 "))
                                 (list (alg "sin(x)")))
                #t)

    (test-equal "EA: Example 6.18 - 5"
                (polynomial-gpe? (automatic-simplify
                                  (alg " x/y + 2 y "))
                                 '(x y))
                #f)

    (test-equal "EA: Example 6.18 - 5"
                (polynomial-gpe? (automatic-simplify
                                  (alg " (x+1) * (x+3) "))
                                 '(x))
                #f)

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; variables
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-equal "EA: Example 6.20 - 1"
                (lset= equal?
                       (variables
                        (automatic-simplify
                         (alg " x^3 + 3 x^2 y + 3 x y^2 + y^3 ")))
                       '(x y))
                #t)

    (test-equal "EA: Example 6.20 - 2"
                (lset= equal?
                       (variables
                        (automatic-simplify
                         (alg " 3 x * (x+1) * y^2 * z^n ")))
                       '((^ z n) y (+ 1 x) x))
                #t)

    (test-equal "EA: Example 6.20 - 3"
                (lset= equal?
                       (variables
                        (automatic-simplify
                         (alg " a sin(x)^2 + 2 b sin(x) + 3 c ")))
                       '(a b (sin x) c))
                #t)

    (test-equal "EA: Example 6.20 - 3"
                (lset= equal?
                       (variables
                        (automatic-simplify
                         (alg " a sin(x)^2 + 2 b sin(x) + 3 c ")))
                       '(a b (sin x) c))
                #t)

    (test-equal "EA: Example 6.20 - 4"
                (variables 1/2)
                '())
                
    (test-equal "EA: Example 6.20 - 5"
                (lset= equal?
                       (variables
                        (automatic-simplify
                         (alg " sqrt(2) * x^2 + sqrt(3) * x + sqrt(5) ")))
                       '((sqrt 2) (sqrt 3) x (sqrt 5)))
                #t)

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; degree-gpe
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-equal "EA: Example 6.22 - 1"
                (degree-gpe
                 (automatic-simplify
                  (alg " 3 w x^2 y^3 z^4 "))
                 '(x z))
                6)

    (test-equal "EA: Example 6.22 - 2"
                (degree-gpe
                 (automatic-simplify
                  (alg " a x^2 + b x + c "))
                 '(x))
                2)
    
    (test-equal "EA: Example 6.22 - 3"
                (degree-gpe
                 (automatic-simplify
                  (alg " a * sin(x)^2 + b * sin(x) + c "))
                 '((sin x)))
                2)

    (test-equal "EA: Example 6.22 - 4"
                (degree-gpe
                 (automatic-simplify
                  (alg " 2 x^2 y z^3 + w x z^6 "))
                 '(x z))
                7)

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (let ()

      (define (total-degree u)
        (degree-gpe u (variables u)))

      (test-equal "EA: Example 6.25"
                  (total-degree (automatic-simplify
                                 (alg " a x^2 + b x + c ")))
                  3))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; coefficient-gpe
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-equal "EA: Example 6.27 - 1"

                (coefficient-gpe (automatic-simplify
                                  (alg " a x^2 + b x + c "))
                                 x
                                 2)

                a)

    (test-equal "EA: Example 6.27 - 2"

                (coefficient-gpe (automatic-simplify
                                  (alg " 3 x y^2 + 5 x^2 y + 7 x + 9 "))
                                 x
                                 1)

                '(+ 7 (* 3 (^ y 2)))
                )

    (test-equal "EA: Example 6.27 - 3"

                (coefficient-gpe (automatic-simplify
                                  (alg " 3 x y^2 + 5 x^2 y + 7 x + 9 "))
                                 x
                                 3)

                0)

    (test-equal "EA: Example 6.27 - 4"

                (coefficient-gpe (automatic-simplify
                                  (alg " 3 * sin(x) * x^2 + 2 * ln(x) * x + 4 "))
                                 x
                                 2)
                'undefined)

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; leading-coefficient-gpe
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-equal "EA: page 233"

                (leading-coefficient-gpe
                 (automatic-simplify
                  (alg " 3 x y^2 + 5 x^2 y + 7 x^2 y^3 + 9 "))
                 x)

                (automatic-simplify
                 (alg " 5 y + 7 y^3 "))

                )

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; coeff-var-monomial
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-equal "coeff-var-monomial 1"

                (coeff-var-monomial (automatic-simplify
                                     (alg "3 x y"))
                                    '(x))

                '((* 3 y) x)

                )

    (test-equal "coeff-var-monomial 2"

                (coeff-var-monomial (automatic-simplify
                                     (alg "3 x y"))
                                    '(y))

                '((* 3 x) y)

                )

    (test-equal "coeff-var-monomial 3"

                (coeff-var-monomial (automatic-simplify
                                     (alg "3 x y"))
                                    '(x y))

                '(3 (* x y))

                )

    (test-equal "coeff-var-monomial 4"

                (coeff-var-monomial (automatic-simplify
                                     (alg "3 x y"))
                                    '(3 x y))

                '(1 (* 3 x y))

                )

    (test-equal "coeff-var-monomial 5"

                (coeff-var-monomial (automatic-simplify
                                     (alg "3 x y"))
                                    '())

                '((* 3 x y) 1)

                )

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; collect-terms
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-equal "EA: Example 6.43"

                (collect-terms (alge " 2 a x y + 3 b x y + 4 a x + 5 b x ")
                               '(x y))

                (alge " (2 a + 3 b) x y + (4 a + 5 b) x ")

                )

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; algebraic-expand
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-equal "EA: 6.34"

                (algebraic-expand (alge " (x+2) * (x+3) * (x+4) "))

                (alge " x^3 + 9 x^2 + 26 x + 24 ")

                )

    (test-equal "EA: 6.35"

                (algebraic-expand (alge " (x+y+z)^3 "))

                (alge " x^3 + y^3 + z^3 +
                        3 x^2 y + 3 x^2 z +
                        3 y^2 x + 3 y^2 z +
                        3 z^2 x + 3 z^2 y +
                        6 x y z " )

                )

    (test-equal "EA: 6.36"

                (algebraic-expand (alge " (x+1)^2 + (y+1)^2 "))

                (alge " x^2 + 2 x + y^2 + 2 y + 2 ")

                )

    (test-equal "EA: 6.37"

                (algebraic-expand (alge " ((x+2)^2 +3)^2 "))

                (alge " x^4 + 8 x^3 + 30 x^2 + 56 x + 49 ")

                )

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; numerator
    ;; denominator
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-equal "EA: Example 6.49 - numerator"

                (numerator (alge " 2/3 * (x*(x+1)) / (x+2) * y^n "))

                (alge " 2 x * (x+1) y^n  "))

    (test-equal "EA: Example 6.49 - denominator"

                (denominator (alge " 2/3 * (x*(x+1)) / (x+2) * y^n "))

                (alge " 3 * ( x + 2 ) "))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; rational-gre?
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-equal "EA: Example 6.54 - 1"

                 (rational-gre? (alge " (x^2 + 1) / (2 x + 3) ") '(x))

                 '(x))

    (test-equal "EA: Example 6.54 - 2"

                 (rational-gre? (alge " 1/x + 1/y ") '(x y))

                 #f)

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; rational-variables
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-equal "EA: Example 6.56 - 1"

                 (rational-variables (alge " (2 x + 3 y) / (z + 4) "))

                 '(z y x))

    (test-equal "EA: Example 6.56 - 2"

                 (rational-variables (alge " 1/x + 1/y "))

                 (list (alge "1/y") (alge "1/x")))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; rationalize-expression
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-equal "EA: page 266 - 1"
                 
                (rationalize-expression (alge " (1 + 1/x)^2 "))

                (alge " (x+1)^2 / x^2 "))

    (test-equal "EA: page 266 - 1"
                 
                (rationalize-expression (alge " (1 + 1/x)^(1/2) "))

                (alge " ( (x+1)/x ) ^ (1/2) "))

    (test-equal "EA: Example 6.59"

                (rationalize-expression
                 (alge " 1 / (1+1/x)^(1/2) + (1+1/x)^(3/2) "))

                (alge " ( x^2 + (x+1)^2 ) / ( x^2 * ( (x+1) / x ) ^ (1/2) ) ")

                )

    (test-equal "EA: Example 6.60"

                (rationalize-expression (alge " a + b/2 "))

                (alge " (2 a + b)/2 ")

                )

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; rational-expand
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-equal "EA: Example 6.62"
    
                (rational-expand
                 (alge
                  " ( ( ( 1 / ( (x+y)^2 + 1 ) ) ^ (1/2) + 1 )
                      *
                      ( ( 1 / ( (x+y)^2 + 1 ) ) ^ (1/2) - 1 ) )
                    /
                    (x+1) "))

                (alge
                 " ( - x^2 - 2 x y - y^2 )
                   /
                   ( x^3 + x^2 + 2 x^2 y + 2 x y + x y^2 + y^2 + x + 1 ) ")

                )

    (test-equal

     "EA: page 269"

     (rational-expand
      (alge
       " 1 / ( 1/a + c / (a b) ) + ( a b c + a c^2 ) / (b+c)^2 - a "))

     0)

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; expand-exp
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-equal "EA: expression 7.1"

                (expand-exp (alge " exp( 2 x + y "))

                (alge " exp(x)^2 exp(y) "))

    (test-equal "EA: Example 7.2"

                (expand-exp (alge " exp( 2 w x + 3 y z "))

                (alge " exp( w x )^2 exp( y z )^3 "))

    (test-equal "EA: Example 7.3"

                (expand-exp (alge " exp( 2 * (x + y) ) "))

                (alge " exp(x)^2 exp(y)^2 "))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; expand-trig
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-equal "EA: Expression 7.11"

                (expand-trig (alge " sin(x+y) "))

                (alge " sin(x) * cos(y) + cos(x) * sin(y) "))

    (test-equal "EA: Expression 7.12"

                (expand-trig (alge " cos(x+y) "))

                (alge " cos(x) * cos(y) - sin(x) * sin(y) "))

    (test-equal "EA: Example 7.5"

                (expand-trig (alge " sin(2x + 3y) "))

                (alge " 2 * sin(x) * cos(x) * ( cos(y)^3 - 3 * cos(y) * sin(y)^2 )
                        +
                        ( cos(x)^2 - sin(x)^2 ) * ( 3 cos(y)^2 sin(y) - sin(y)^3 ) "))

    (test-equal "EA: Example 7.6"
    
                (expand-trig (alge " sin( 2 * (x+y) ) "))

                (alge " 2 * ( sin(x) * cos(y) + cos(x) * sin(y) ) *
                        ( cos(x) * cos(y) - sin(x) * sin(y) ) "))

    (test-equal "EA: Expression 7.18"
    
                (expand-trig (alge " cos(5x) "))

                (alge "cos(x)^5 - 10 * cos(x)^3 * sin(x)^2 + 5 * cos(x) * sin(x)^4"))
    
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (test-end "mpl")

    )

  )
  