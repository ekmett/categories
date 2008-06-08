{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Morphism.Zygo 
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
--
----------------------------------------------------------------------------
module Control.Morphism.Zygo 
	( Zygo, ZygoT
	, distZygo, distZygoT
	, zygo
	, g_zygo
	, prepro_zygo
	, g_prepro_zygo 
	) where

import Control.Arrow ((&&&))
import Control.Comonad
import Control.Comonad.Reader
import Control.Functor.Algebra
import Control.Functor.Extras
import Control.Functor.Fix
import Control.Morphism.Cata
import Control.Morphism.Prepro

type Zygo = (,)
type ZygoT = CoreaderT

-- * Distributive Law Combinators

distZygo :: Functor f => Algebra f b -> Dist f (Zygo b)
distZygo g = g . fmap fst &&& fmap snd

distZygoT :: (Functor f, Comonad w) => GAlgebra f w b -> Dist f w -> Dist f (ZygoT w b)
distZygoT g k = CoreaderT . liftW (g . fmap (liftW fst) &&& fmap (snd . extract)) . k . fmap (duplicate . runCoreaderT)

zygo :: Functor f => Algebra f b -> GAlgebra f (Zygo b) a -> FixF f -> a
zygo f = g_cata (distZygo f)

g_zygo :: (Functor f, Comonad w) => GAlgebra f w b -> Dist f w -> GAlgebra f (ZygoT w b) a -> FixF f -> a
g_zygo f w = g_cata (distZygoT f w)

-- | a zygomorphic prepromorphism
prepro_zygo :: Functor f => Algebra f b -> GAlgebra f (Zygo b) a -> (f :~> f) -> FixF f -> a
prepro_zygo f = g_prepro (distZygo f)

-- | a generalized zygomorphic prepromorphism 
g_prepro_zygo :: (Functor f, Comonad w) => GAlgebra f w b -> Dist f w -> GAlgebra f (ZygoT w b) a -> (f :~> f) -> FixF f -> a
g_prepro_zygo f w = g_prepro (distZygoT f w)
