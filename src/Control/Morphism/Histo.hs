{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Morphism.Histo 
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
-- 
-- Traditional operators, shown here to show how to roll your own
----------------------------------------------------------------------------
module Control.Morphism.Histo where

import Control.Functor.Algebra
import Control.Functor.Extras
import Control.Functor.Fix
import Control.Comonad
import Control.Comonad.Cofree
import Control.Morphism.Cata

histo :: Functor f => GAlgebra f (Cofree f) a -> FixF f -> a
histo = g_cata (distHisto id)

g_histo :: (Functor f, Functor h) => Dist f h -> GAlgebra f (Cofree h) a -> FixF f -> a
g_histo k = g_cata (distHisto k)

distHisto :: (Functor f, Functor h) => Dist f h -> Dist f (Cofree h)
distHisto k = anaCofree (fmap extract) (k . fmap outCofree)
