
(library (mpl misc)

  (export product? quotient? sum? difference? power? factorial? function?
          exp?
          vars
          )

  (import (rnrs)
          (xitomatl AS-match))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (product? expr)
    (match expr
      ( ('* . elts) #t )
      ( else        #f )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (quotient? expr)
    (match expr
      ( ('/ . elts) #t )
      ( else        #f )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (sum? expr)
    (match expr
      ( ('+ . elts) #t )
      ( else        #f )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (difference? expr)
    (match expr
      ( ('- . elts) #t )
      ( else        #f )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (power? expr)
    (match expr
      ( ('^ x y) #t )
      ( else     #f )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (factorial? expr)
    (match expr
      ( ('! n) #t )
      ( else   #f )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (exp? expr)
    (match expr
      ( ('exp n) #t )
      ( else     #f )))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (function? expr)
    (and (list? expr)
         (> (length expr) 1)
         (symbol? (car expr))
         (not (member (car expr)
                      '(+ - * / ^ !)))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax vars
    (syntax-rules ()
      ((vars name ...)
       (begin (define name 'name)
              ...))))

  )