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
module Control.Morphism.Para 
	( Para
	, ParaT 
	, distParaT 
	, para, g_para
	, prepro_para, g_prepro_para
	) where

import Control.Comonad
import Control.Comonad.Reader
import Control.Functor.Algebra
import Control.Functor.Extras
import Control.Functor.Fix
import Control.Morphism.Cata
import Control.Morphism.Zygo
import Control.Morphism.Prepro

-- * Paramorphisms use Reader Comonads
type Para f 	= (,) (FixF f)
type ParaT w f 	= CoreaderT w (FixF f)

-- * Distributive Laws
distParaT :: (Functor f, Comonad w) => Dist f w -> Dist f (ParaT w f)
distParaT = distZygoT (liftAlgebra InF)

-- * Paramorphism
para :: Functor f => GAlgebra f (Para f) a -> FixF f -> a
para = zygo InF

-- | Generalized paramorphisms using a comonad reader transformer to carry the primitive recursive state
g_para :: (Functor f, Comonad w) => Dist f w -> GAlgebra f (ParaT w f) a -> FixF f -> a
g_para f = g_cata (distParaT f)

-- | A paramorphic prepromorphism
prepro_para :: Functor f => GAlgebra f (Para f) a -> (f :~> f) -> FixF f -> a
prepro_para = prepro_zygo InF

-- | A generalized paramorphic prepromorphism
g_prepro_para :: (Functor f, Comonad w) => Dist f w -> GAlgebra f (ParaT w f) a -> (f :~> f) -> FixF f -> a
g_prepro_para f = g_prepro (distParaT f)
