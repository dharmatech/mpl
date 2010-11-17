#!r6rs

(library (mpl contract-exp)

  (export contract-exp)

  (import (mpl rnrs-sans)
          (mpl misc)
          (mpl arithmetic)
          (mpl exp)
          (mpl expand-main-op))

  (define (contract-exp-rules u)

    (let ((v (expand-main-op u)))

      (cond ( (power? v)

              (let ((b (list-ref v 1))
                    (s (list-ref v 2)))

                (if (exp? b)
                    (let ((p (* (list-ref b 1) s)))
                      (if (or (product? p)
                              (power? p))
                          (exp (contract-exp-rules p))
                          (exp p)))
                    v)) )

            ( (product? v)

              (let ((p 1)
                    (s 0))

                (for-each
                 (lambda (y)
                   (if (exp? y)
                       (set! s (+ s (list-ref y 1)))
                       (set! p (* p y))))
                 (cdr v))

                (* (exp s) p)) )

            ( (sum? v)

              (let ((s 0))

                (for-each
                 (lambda (y)
                   (if (or (product? y)
                           (power?   y))
                       (set! s (+ s (contract-exp-rules y)))
                       (set! s (+ s y))))
                 (cdr v))

                s) )

            ( else v ))))

  (define (contract-exp u)

    (if (or (number? u)
            (symbol? u))
        u
        (let ((v (map contract-exp u)))
          (if (or (product? v)
                  (power?   v))
              (contract-exp-rules v)
              v))))

  )