module Fsuma where

suma [] =0
suma (n:ns) = n + suma ns

{- Show that sum [x] = x for any number x.
	sum [x]		
			=	x + sum []
		  	=	x + 0
		  	=	x