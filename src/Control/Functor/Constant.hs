{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Constant
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (functional-dependencies)
--
-------------------------------------------------------------------------------------------

module Control.Functor.Constant where

import Control.Functor.Exponential
import Control.Functor.Contravariant
import Data.Void

-- this is also the 'Fst' bifunctor

newtype ConstantF a b = ConstantF a

instance Functor (ConstantF a) where
	fmap _ (ConstantF a) = ConstantF a

instance ContravariantFunctor (ConstantF a) where
	contramap _ (ConstantF a) = ConstantF a

instance ExpFunctor (ConstantF a) where
	xmap _ _ (ConstantF a) = ConstantF a

type VoidF a = ConstantF Void a
