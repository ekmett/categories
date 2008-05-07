-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Bifunctor.Constant
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD3
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-------------------------------------------------------------------------------------------

module Control.Bifunctor.Constant 
	( module Control.Functor.Exponential
	, module Control.Functor.Contravariant
	, module Control.Bifunctor.Associative
	, ConstB(..)
	, FstB(..)
	, SndB(..)
	) where

import Control.Bifunctor.Associative
import Control.Functor.Exponential
import Control.Functor.Contravariant

newtype ConstB t a b = ConstB { runConstB :: t } 

instance Bifunctor (ConstB t) where
	bimap _ _ = ConstB . runConstB

instance Functor (ConstB t a) where
	fmap _ = ConstB . runConstB

instance ContravariantFunctor (ConstB t a) where
	contramap _ = ConstB . runConstB

instance ExpFunctor (ConstB t a) where
	xmap _ _ = ConstB . runConstB






newtype FstB a b = FstB { runFstB :: a } 

instance Bifunctor FstB where
	bimap f _ = FstB . f . runFstB 

instance Associative FstB where
	associate = FstB . runFstB . runFstB

instance Functor (FstB a) where
        fmap _ (FstB a) = FstB a

instance ContravariantFunctor (FstB a) where
        contramap _ (FstB a) = FstB a

instance ExpFunctor (FstB a) where
        xmap _ _ (FstB a) = FstB a








newtype SndB a b = SndB { runSndB :: b } 

instance Bifunctor SndB where
	bimap _ g = SndB . g . runSndB 

instance Functor (SndB a) where
	fmap = bimap id
