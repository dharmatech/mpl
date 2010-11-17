#!r6rs

(library (mpl util)

  (export while)

  (import (rnrs))

  (define-syntax while
    (syntax-rules ()
      ((while test expr ...)
       (let loop ()
         (when test
           expr
           ...
           (loop)))))))
           