#!r6rs

(library (mpl util symbols)

 (export symbol<?)

 (import (rnrs))

 (define (symbol<? a b)
   (string<? (symbol->string a)
             (symbol->string b))))