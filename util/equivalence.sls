#!r6rs

(library (mpl util equivalence)

 (export equal-to eq-to eqv-to)

 (import (rnrs)
         (mpl util extended-curry))
 
 (define equal-to (curry equal? a b))

 (define eq-to (curry equal? a b))

 (define eqv-to (curry eqv? a b)))