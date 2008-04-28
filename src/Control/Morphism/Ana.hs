{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Morphism.Ana
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
-- 
-- Traditional operators, shown here to show how to roll your own
----------------------------------------------------------------------------
module Control.Morphism.Ana where

import Control.Functor.Algebra
import Control.Functor.Extras
import Control.Functor.Fix
import Control.Comonad
import Control.Monad.Identity
import Control.Comonad.Identity

-- * Unfold Sugar

ana :: Functor f => CoAlg f a -> a -> Nu f
-- ana g = g_ana distAna (liftCoAlg g)
ana g = InF . fmap (ana g) . g

g_ana :: (Functor f, Monad m) => Dist m f -> CoAlgM f m a -> a -> Nu f
-- g_ana k g = g_hylo distCata k inW id g
g_ana k g = a . return where a = InF . fmap (a . join) . k . liftM g

distAna :: Functor f => Dist Identity f
distAna = fmap Identity . runIdentity

