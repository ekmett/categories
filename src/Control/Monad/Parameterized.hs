{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Paramterized
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Monad.Parameterized where

import Control.Arrow ((|||), (+++))
import Control.Monad
import Control.Bifunctor
import Control.Bifunctor.Fix
import Control.Monad.Parameterized.Class
import Control.Morphism.Cata

paugment :: PMonad f => (forall c. (f a c -> c) -> c) -> (a -> FixB f b) -> FixB f b
paugment g k = g (InB . pbind (outB . k))

instance PMonad f => Monad (FixB f) where
	return = InB . preturn
	m >>= k = paugment (flip bicata m) k

