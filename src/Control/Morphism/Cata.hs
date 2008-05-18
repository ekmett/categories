{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Morphism.Cata
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
-- 
----------------------------------------------------------------------------
module Control.Morphism.Cata where

import Control.Comonad
import Control.Category.Hask
import Control.Functor
import Control.Functor.Pointed
import Control.Functor.Algebra 
import Control.Functor.HigherOrder
import Control.Functor.Extras
import Control.Functor.Fix
import Control.Monad.Identity

cata :: Functor f => Alg f a -> FixF f -> a
cata f = f . fmap (cata f) . outF
-- cata f = g_cata distCata (liftAlg f)

g_cata :: (Functor f, Comonad w) => Dist f w -> AlgW f w a -> FixF f -> a
g_cata k g = extract . c where c = liftW g . k . fmap (duplicate . c) . outF
-- g_cata k f = g_hylo k distAna f id outM

distCata :: Functor f => Dist f Identity
distCata = Identity . fmap runIdentity

bicata :: Bifunctor f Hask Hask Hask => Alg (f b) a -> Fix f b -> a
bicata f = f . bimap id (bicata f) . outB

g_bicata :: (Bifunctor f Hask Hask Hask, Comonad w) => Dist (f b) w -> AlgW (f b) w a -> Fix f b -> a
g_bicata k g = extract . c where c = liftW g . k . bimap id (duplicate . c) . outB

hcata :: HFunctor f => AlgH f a -> FixH f :~> a
hcata f = f . hfmap (hcata f) . outH
