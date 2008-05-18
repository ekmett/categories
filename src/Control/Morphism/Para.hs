{-# OPTIONS_GHC -fglasgow-exts #-}
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
import Control.Functor.Algebra
import Control.Functor.Extras
import Control.Functor.Fix
import Control.Morphism.Cata
import Control.Morphism.Zygo

-- * Refold Sugar

para :: Functor f => AlgW f (Para f) a -> FixF f -> a
para = zygo InF

g_para :: (Functor f, Comonad w) => Dist f w -> AlgW f (ParaT w f) a -> FixF f -> a
g_para f = g_cata (distParaT f)

type Para f a 		= (FixF f, a)
type ParaT w f a 	= CoreaderT w (FixF f) a

distParaT :: (Functor f, Comonad w) => Dist f w -> Dist f (ParaT w f)
distParaT = distZygoT (liftAlg InF)
