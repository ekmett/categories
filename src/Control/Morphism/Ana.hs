{-# OPTIONS_GHC -fglasgow-exts #-}
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

import Control.Category.Hask
import Control.Functor
import Control.Functor.Algebra
import Control.Functor.Extras
import Control.Functor.Fix
import Control.Functor.HigherOrder
import Control.Comonad ()
import Control.Monad.Identity

-- | Anamorphisms are a generalized form of 'unfoldr'
ana :: Functor f => CoAlg f a -> a -> FixF f
ana g = InF . fmap (ana g) . g
-- ana g = g_ana distAna (liftCoAlg g)

-- | Generalized anamorphisms allow you to work with a monad given a distributive law
g_ana :: (Functor f, Monad m) => Dist m f -> CoAlgM f m a -> a -> FixF f
-- g_ana k g = g_hylo distCata k inW id g
g_ana k g = a . return where a = InF . fmap (a . join) . k . liftM g

-- | The distributive law for the identity monad
distAna :: Functor f => Dist Identity f
distAna = fmap Identity . runIdentity

biana :: Bifunctor f Hask Hask Hask => CoAlg (f b) a -> a -> Fix f b
biana g = InB . bimap id (biana g) . g

g_biana :: (Bifunctor f Hask Hask Hask, Monad m) => Dist m (f b) -> CoAlgM (f b) m a -> a -> Fix f b
g_biana k g = a . return where a = InB . bimap id (a . join) . k . liftM g

-- | A higher-order anamorphism for constructing higher order functors.
hana :: HFunctor f => CoAlgH f a -> a :~> FixH f
hana g = InH . hfmap (hana g) . g
