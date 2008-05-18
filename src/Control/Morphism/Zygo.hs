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
module Control.Morphism.Zygo where

import Control.Arrow ((&&&))
import Control.Comonad
import Control.Comonad.Reader
import Control.Functor.Algebra
import Control.Functor.Extras
import Control.Functor.Fix
import Control.Morphism.Cata

type Zygo b a = (b,a)

zygo :: Functor f => Alg f b -> AlgW f (Zygo b) a -> FixF f -> a
zygo f = g_cata (distZygo f)

g_zygo :: (Functor f, Comonad w) => AlgW f w b -> Dist f w -> AlgW f (CoreaderT w b) a -> FixF f -> a
g_zygo f w = g_cata (distZygoT f w)

-- * Distributive Law Combinators

distZygo :: Functor f => Alg f b -> Dist f (Zygo b)
distZygo g = g . fmap fst &&& fmap snd

distZygoT :: (Functor f, Comonad w) => AlgW f w b -> Dist f w -> Dist f (CoreaderT w b)
distZygoT g k = CoreaderT . liftW (g . fmap (liftW fst) &&& fmap (snd . extract)) . k . fmap (duplicate . runCoreaderT)

