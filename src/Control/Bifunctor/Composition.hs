-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Bifunctor.Composition
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD3
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-------------------------------------------------------------------------------------------

module Control.Bifunctor.Composition 
	( module Control.Bifunctor.Braided
	, CompB(..)
	, liftCompB
	) where

import Control.Bifunctor.Braided

newtype CompB p f g a b = CompB { runCompB :: p (f a b) (g a b) }

instance (Bifunctor p, Bifunctor f, Bifunctor g) => Bifunctor (CompB p f g) where
	bimap f g = CompB . bimap (bimap f g) (bimap f g) . runCompB

liftCompB :: Bifunctor p => (f a b -> f c d) -> (g a b -> g c d) -> CompB p f g a b -> CompB p f g c d 
liftCompB f g = CompB . bimap f g . runCompB

instance (Bifunctor p, Braided f, Braided g) => Braided (CompB p f g) where
	braid = liftCompB braid braid

instance (Bifunctor p, Symmetric f, Symmetric g) => Symmetric (CompB p f g) 

instance (Bifunctor p, Bifunctor f, Bifunctor g) => Functor (CompB p f g a) where
	fmap = bimap id


