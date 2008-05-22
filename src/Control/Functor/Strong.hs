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

import Prelude hiding (sequence,Either)
import Data.Traversable
import Control.Monad.Either (Either(..))

strength :: Functor f => a -> f b -> f (a,b)
strength a fb = fmap ((,)a) fb

costrength :: Traversable f => f (Either a b) -> Either a (f b)
costrength = sequence
