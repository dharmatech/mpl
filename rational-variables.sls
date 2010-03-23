
(library (mpl rational-variables)

  (export rational-variables)

  (import (except (rnrs) numerator denominator exp)
          (only (surfage s1 lists) lset-union)
          (mpl variables)
          (mpl numerator)
          (mpl denominator)
          )

  (define (rational-variables u)
    (lset-union equal?
                (variables (numerator   u))
                (variables (denominator u))))

  )
                
    
          