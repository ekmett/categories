{-# OPTIONS -fglasgow-exts #-}
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


import Control.Functor.Algebra
import Control.Functor.Extras
import Control.Functor.Fix
import Control.Comonad
import Control.Comonad.Reader
import Control.Comonad.Instances
import Control.Morphism.Cata
import Control.Arrow ((&&&))

type Zygo b a = (b,a)

zygo :: Functor f => Alg f b -> AlgW f (Zygo b) a -> Mu f -> a
zygo f = g_cata (distZygo f)

g_zygo :: (Functor f, Comonad w) => AlgW f w b -> Dist f w -> AlgW f (ReaderCT b w) a -> Mu f -> a
g_zygo f w = g_cata (distZygoT f w)

-- * Distributive Law Combinators

distZygo :: Functor f => Alg f b -> Dist f (Zygo b)
distZygo g = g . fmap fst &&& fmap snd

distZygoT :: (Functor f, Comonad w) => AlgW f w b -> Dist f w -> Dist f (ReaderCT b w)
distZygoT g k = ReaderCT . liftW (g . fmap (liftW fst) &&& fmap (snd . extract)) . k . fmap (duplicate . runReaderCT)

