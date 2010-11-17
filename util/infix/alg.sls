#!r6rs

(library (mpl util infix alg)

  (export alg string->infix)

  (import (rnrs)
          (rnrs r5rs)
          (rnrs mutable-strings)
          (mpl util infix tokenizer)
          (mpl util infix infix))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (parse-eof) #f)

  (define (parse-comma) #\,)

  (define (string->infix str)

    (define (expr)

      (let loop ()

        (let ((token (lexer)))

          (cond ((eq? token #\()
                 (let ((a (expr)))
                   (let ((b (loop)))
                     (cons a b))))

                ((eq? token #\))
                 '())

                ((eq? token #f)
                 '())

                (else (cons token (loop)))))))

    (lexer-init 'string str)

    (expr))

  (define (alg str)
    (infix (string->infix str)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Examples
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; (alg "a+b-c*d/e=f")

  ;; (alg "(a-b)/(c+d)")

  ;; (alg "a+b+c")

  ;; (alg "sin (cos (x)")

  ;; (alg "2 a b + 3 b c")

  ;; (alg "abc(x) + def(y)")

  ;; (alg "f(x,y) + g(i,j)")

  )