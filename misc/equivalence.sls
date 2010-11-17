#!r6rs

(library (mpl misc equivalence)

 (export equal-to eq-to eqv-to)

 (import (rnrs)
         (mpl misc extended-curry))
 
 (define equal-to (curry equal? a b))

 (define eq-to (curry equal? a b))

 (define eqv-to (curry eqv? a b)))