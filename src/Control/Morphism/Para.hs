{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Morphism.Para
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
-- 
----------------------------------------------------------------------------
module Control.Morphism.Para where

import Control.Comonad
import Control.Comonad.Reader
import Control.Comonad.Instances
import Control.Functor.Algebra
import Control.Functor.Extras
import Control.Functor.Fix
import Control.Morphism.Cata
import Control.Morphism.Zygo

-- * Refold Sugar

para :: Functor f => AlgW f (Para f) a -> Mu f -> a
para = zygo InF

g_para :: (Functor f, Comonad w) => Dist f w -> AlgW f (ParaT f w) a -> Mu f -> a
g_para f = g_cata (distParaT f)

type Para f a 		= (Mu f, a)
type ParaT f w a 	= ReaderCT (Mu f) w a

distParaT :: (Functor f, Comonad w) => Dist f w -> Dist f (ParaT f w)
distParaT = distZygoT (liftAlg InF)
