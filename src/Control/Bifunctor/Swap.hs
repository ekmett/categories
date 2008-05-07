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

module Control.Bifunctor.Swap 
	( module Control.Bifunctor.Monoidal
	, SwapB(..)
	, liftSwapB
	) where

import Control.Bifunctor.Monoidal

newtype SwapB p a b = SwapB { runSwapB :: p b a } 

liftSwapB :: Bifunctor p => (p a b -> p c d) -> SwapB p b a -> SwapB p d c
liftSwapB f = SwapB . f . runSwapB

instance Bifunctor p => Bifunctor (SwapB p) where
	bimap f g = liftSwapB (bimap g f)

instance Braided p => Braided (SwapB p) where
	braid = liftSwapB braid

instance Symmetric p => Symmetric (SwapB p)

instance Bifunctor p => Functor (SwapB p a) where
	fmap = bimap id

