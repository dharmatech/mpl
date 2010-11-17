#!r6rs

(library (mpl arithmetic)

  (export + - * / ^)

  (import (mpl sum-product-power)
          (mpl sub)
          (mpl div)))