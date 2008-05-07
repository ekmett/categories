{-# OPTIONS_GHC -cpp #-}

-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Arrow.CoKleisli
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD3
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-------------------------------------------------------------------------------------------
module Control.Arrow.CoKleisli 
	( CoKleisli(..)
	) where

#if __GLASGOW_HASKELL__ >= 609
import Prelude hiding (id,(.))
import Control.Category
#endif
import Control.Comonad
import Control.Arrow

newtype CoKleisli w a b = CoKleisli { runCoKleisli :: w a -> b }

instance Functor (CoKleisli w a) where
	fmap f (CoKleisli g) = CoKleisli (f . g)

instance Comonad w => Arrow (CoKleisli w) where
	arr f = CoKleisli (f . extract)
	CoKleisli a &&& CoKleisli b = CoKleisli (a &&& b)
	CoKleisli a *** CoKleisli b = CoKleisli (a . fmap fst &&& b . fmap snd)
	first a = a *** CoKleisli extract
	second a = CoKleisli extract *** a
#if __GLASGOW_HASKELL__ >= 609
instance Comonad w => Category (CoKleisli w) where
	id = CoKleisli extract
	CoKleisli b . CoKleisli a = CoKleisli (b . fmap a . duplicate)
#else
	CoKleisli a >>> CoKleisli b = CoKleisli (b . fmap a . duplicate)
#endif

