-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Abides
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD3
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-- See p20 of <http://dbappl.cs.utwente.nl/Publications/PaperStore/db-utwente-404F4540.pdf>
-- TODO: Encode the Eckmann-Hilton argument
-------------------------------------------------------------------------------------------

module Control.Functor.Abides
	( Abides(..)
	) where

import Control.Functor

-- Given p = (-) and q = (|), p and q abide (per Bird) when:

-- a | b   a | b
-- ----- = - | - 
-- c | d   c | d


class Abides p q where
	abide :: q (p a b) (p c d) -> p (q a c) (p b d)
	unabide :: p (q a c) (p b d) -> q (p a b) (p c d)

instance Abides (,) (,) where
	abide ((a,b),(c,d)) = ((a,c),(b,d))
	unabide = abide

instance Abides Either Either where
	abide (Left (Left a)) 	= Left (Left a)
	abide (Left (Right b)) 	= Right (Left b)
	abide (Right (Left c)) 	= Left (Right c)
	abide (Right (Right d)) = Right (Right d)
