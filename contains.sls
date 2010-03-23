
(library (mpl contains)

  (export contains?
          this-contains?
          contains-this?
          free?
          is-free?
          free-of?)

  (import (rnrs)
          (only (surfage s1 lists) any))

  (define (contains-this? t)
    (lambda (u)
      (contains? u t)))

  (define (this-contains? u)
    (lambda (t)
      (contains? u t)))

  (define (contains? u t)
    (or (equal? u t)
        (and (list? u)
             (any (contains-this? t) u))))

  (define (free? u t)
    (not (contains? u t)))

  (define (is-free? u)
    (lambda (t)
      (free? u t)))

  (define (free-of? t)
    (lambda (u)
      (free? u t)))

  )