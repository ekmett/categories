{-# OPTIONS -fglasgow-exts #-}
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

import Control.Functor.Algebra 
import Control.Functor.Extras
import Control.Functor.Fix
import Control.Comonad
import Control.Monad.Identity
import Control.Comonad.Identity

cata :: Functor f => Alg f a -> Mu f -> a
-- cata f = g_cata distCata (liftAlg f)
cata f = f . fmap (cata f) . outF

g_cata :: (Functor f, Comonad w) => Dist f w -> AlgW f w a -> Mu f -> a
-- g_cata k f = g_hylo k distAna f id outM
g_cata k g = extract . c where c = liftW g . k . fmap (duplicate . c) . outF

distCata :: Functor f => Dist f Identity
distCata = Identity . fmap runIdentity
