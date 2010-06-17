-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Contra
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-------------------------------------------------------------------------------------------

module Control.Functor.Contra
	( ContraFunctor(..)
	, ContraF(..)
	) where

import Control.Applicative 

class ContraFunctor f where
	contramap :: (a -> b)  -> f b -> f a

newtype ContraF a b = ContraF { runContraF :: b -> a }

instance ContraFunctor (ContraF a) where
        contramap g (ContraF f) = ContraF (f . g)

instance ContraFunctor (Const a) where
        contramap _ (Const a) = Const a
