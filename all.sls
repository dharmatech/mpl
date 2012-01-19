#!r6rs

(library (mpl all)

  (export + - * / ^
	  sqrt
	  vars
	  alge
	  substitute
	  collect-terms
	  algebraic-expand
	  expand-exp
	  expand-trig
	  contract-exp
	  contract-trig
	  derivative
	  polynomial-division
	  polynomial-expansion)

  (import (mpl misc)
	  (mpl contains)
	  (mpl sum-product-power)
	  (mpl sub)
	  (mpl div)
	  (mpl exp)
	  (mpl factorial)
	  (mpl numerator)
	  (mpl denominator)
	  (mpl sqrt)
	  (mpl sin)
	  (mpl cos)
	  (mpl tan)
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
	  (mpl expand-main-op)
	  (mpl rational-gre)
	  (mpl rational-variables)
	  (mpl rationalize-expression)
	  (mpl rational-expand)
	  (mpl expand-exp)
	  (mpl expand-trig)
	  (mpl contract-exp)
	  (mpl separate-sin-cos)
	  (mpl contract-trig)
	  (mpl trig-substitute)
	  (mpl simplify-trig)
	  (mpl derivative)
	  (mpl polynomial-division)
	  (mpl polynomial-expansion)
	  (mpl polynomial-gcd)
	  (mpl extended-euclidean-algorithm)
	  (mpl alg-polynomial-division)
	  (mpl alg-polynomial-gcd)
	  (mpl partial-fraction-1)))
