{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Strong
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (functional-dependencies)
--
-------------------------------------------------------------------------------------------

module Control.Functor.Strong where

import Prelude hiding (sequence)
import Data.Traversable
import Control.Monad.Either

strength :: Functor f => f a -> b -> f (a,b)
strength fa b = fmap (\a -> (a,b)) fa

costrength :: Traversable f => f (Either a b) -> Either a (f b)
costrength = sequence
