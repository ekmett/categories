-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Bifunctor.Functor
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD3
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-------------------------------------------------------------------------------------------

module Control.Bifunctor.Functor 
	( module Control.Bifunctor.Monoidal
	, FunctorB(..)
	, liftFunctorB
	) where

import Control.Bifunctor.Monoidal

-- a functor composed around a bifunctor

newtype FunctorB f p a b = FunctorB { runFunctorB :: f (p a b) } 

liftFunctorB :: Functor f => (p a b -> p c d) -> FunctorB f p a b -> FunctorB f p c d
liftFunctorB f = FunctorB . fmap f . runFunctorB

instance (Functor f, Bifunctor p) => Bifunctor (FunctorB f p) where
	bimap f g = liftFunctorB (bimap f g)

instance (Functor f, Braided p) => Braided (FunctorB f p) where
	braid = liftFunctorB braid

instance (Functor f, Symmetric p) => Symmetric (FunctorB f p) 

instance (Functor f, Bifunctor p) => Functor (FunctorB f p a) where
	fmap = bimap id

