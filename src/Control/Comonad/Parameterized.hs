{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Parameterized
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Comonad.Parameterized where

import Control.Bifunctor
import Control.Bifunctor.Fix
import Control.Comonad
import Control.Comonad.Parameterized.Class
import Control.Morphism.Ana

-- this does not seem to be nicely quantifiable
copaugment :: PComonad f => ((FixB f a -> f b (FixB f a)) -> FixB f b) -> (FixB f a -> b) -> FixB f b
copaugment g k = g (pextend (k . InB) . outB)

instance PComonad f => Comonad (FixB f) where
        extract = pextract . outB
        extend k w = copaugment (flip biana w) k
