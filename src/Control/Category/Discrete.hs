{-# LANGUAGE GADTs #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Discrete
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-------------------------------------------------------------------------------------------
module Control.Category.Discrete
	( Discrete(Refl)
	, mapDiscrete
	, cast
	, invDiscrete
	) where

import Prelude hiding (id,(.))
import Control.Category
import Unsafe.Coerce (unsafeCoerce)
-- import Control.Functor.Categorical

data Discrete a b where 
	Refl :: Discrete a a

instance Category Discrete where
	id = Refl
	Refl . Refl = Refl

-- instance CFunctor f Discrete Discrete where cmap = mapDiscrete

mapDiscrete :: Discrete a b -> Discrete (f a) (f b)
mapDiscrete Refl = Refl

cast :: Discrete a b -> a -> b
cast Refl = unsafeCoerce

invDiscrete :: Discrete a b -> Discrete b a
invDiscrete Refl = Refl
