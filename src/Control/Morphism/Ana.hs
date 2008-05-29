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
module Control.Morphism.Ana 
	( ana, g_ana, distAna
	, biana, g_biana
	, hana
	, kana, runkana
	) where

import Control.Category.Hask
import Control.Functor
import Control.Functor.Algebra
import Control.Functor.Extras
import Control.Functor.Fix
import Control.Functor.HigherOrder
import Control.Functor.KanExtension
import Control.Functor.KanExtension.Interpreter
import Control.Comonad ()
import Control.Monad.Identity

-- | Anamorphisms are a generalized form of 'unfoldr'
ana :: Functor f => Coalgebra f a -> a -> FixF f
ana g = InF . fmap (ana g) . g
-- ana g = g_ana distAna (liftCoAlgebra g)

-- | Generalized anamorphisms allow you to work with a monad given a distributive law
g_ana :: (Functor f, Monad m) => Dist m f -> GCoalgebra f m a -> a -> FixF f
-- g_ana k g = g_hylo distCata k inW id g
g_ana k g = a . return where a = InF . fmap (a . join) . k . liftM g

-- | The distributive law for the identity monad
distAna :: Functor f => Dist Identity f
distAna = fmap Identity . runIdentity

biana :: Bifunctor f Hask Hask Hask => Coalgebra (f b) a -> a -> Fix f b
biana g = InB . bimap id (biana g) . g

g_biana :: (Bifunctor f Hask Hask Hask, Monad m) => Dist m (f b) -> GCoalgebra (f b) m a -> a -> Fix f b
g_biana k g = a . return where a = InB . bimap id (a . join) . k . liftM g

-- | A higher-order anamorphism for constructing higher order functors.
hana :: HFunctor f => HCoalgebra f a -> a :~> FixH f
hana g = InH . hfmap (hana g) . g

kana :: HFunctor f => CointerpreterT f g h -> Lan g h :~> FixH f
kana i = hana (cointerpreterCoalgebra i)

runkana :: HFunctor f => CointerpreterT f g h -> (g b -> a) -> h b -> FixH f a 
runkana i f v = kana i (Lan f v)
