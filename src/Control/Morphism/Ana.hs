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
----------------------------------------------------------------------------
module Control.Morphism.Ana where

import Control.Functor.Algebra
import Control.Functor.Extras
import Control.Functor.Fix
import Control.Functor.HigherOrder
import Control.Comonad ()
import Control.Monad.Identity
import Control.Comonad.Identity ()

-- * Unfold Sugar

-- | Anamorphisms are a generalized form of 'unfoldr'
ana :: Functor f => CoAlg f a -> a -> Nu f
ana g = InF . fmap (ana g) . g
-- ana g = g_ana distAna (liftCoAlg g)

-- | Generalized anamorphisms allow you to work with a monad given a distributive law
g_ana :: (Functor f, Monad m) => Dist m f -> CoAlgM f m a -> a -> Nu f
-- g_ana k g = g_hylo distCata k inW id g
g_ana k g = a . return where a = InF . fmap (a . join) . k . liftM g

-- | The distributive law for the identity monad
distAna :: Functor f => Dist Identity f
distAna = fmap Identity . runIdentity

-- | A higher-order anamorphism for constructing higher order functors.
anaH :: HFunctor f => CoAlgH f a -> Natural a (NuH f)
anaH g = InH . hfmap (anaH g) . g
