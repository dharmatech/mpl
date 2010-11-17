#!r6rs

(library (mpl misc)

  (export product? quotient? sum? difference? power? factorial? function?
          exp?
          sin?
          cos?
          tan?
          vars
          base
          exponent
          inexact-number?)

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

  (define (sin? expr)
    (and (pair? expr)
         (eq? (car expr) 'sin)))

  (define (cos? expr)
    (and (pair? expr)
         (eq? (car expr) 'cos)))

  (define (tan? expr)
    (and (pair? expr)
         (eq? (car expr) 'tan)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (function? expr)
    (and (list? expr)
         (> (length expr) 1)
         (symbol? (car expr))
         (not (member (car expr)
                      '(+ - * / ^ !)))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (base expr)
    (if (power? expr)
        (list-ref expr 1)
        expr))

  (define (exponent expr)
    (if (power? expr)
        (list-ref expr 2)
        1))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax vars
    (syntax-rules ()
      ((vars name ...)
       (begin (define name 'name)
              ...))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (inexact-number? x)
    (and (number? x)
         (inexact? x)))

  )