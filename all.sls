#!r6rs

(library (mpl all)
  (export + - * / ^)

  (import (mpl sum-product-power)
	  (mpl sub)
	  (mpl div)
	  (mpl sin)
	  (mpl cos)
	  (mpl tan)))
