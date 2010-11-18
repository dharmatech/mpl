#!r6rs

(library (mpl factorial)

  (export ! simplify-factorial)

  (import (rename (rnrs) (- rnrs:-) (* rnrs:*))
          (mpl match))

  (define (factorial n)
    (if (= n 0)
        1
        (rnrs:* n (factorial (rnrs:- n 1)))))

  (define (simplify-factorial u)
    (match u
      ( ('! (? number? n)) (factorial n) )
      ( ('! n) u )))

  (define (! n)
    (simplify-factorial `(! ,n)))

  )