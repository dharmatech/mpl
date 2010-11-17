#!r6rs

(library (mpl log)

  (export log)

  (import (rename (rnrs) (log rnrs:log))
          (mpl misc))

  (define log

    (case-lambda

     ( (x)

       (cond ( (number? x)

               (rnrs:log x) )

             ( (exp? x) (list-ref x 1) )

             ( else `(log ,x) )) )

     ( (x y)

       (cond ( (and (number? x) (number? y))

               (rnrs:log x y) )

             ( else `(log ,x ,y) )) ))))

