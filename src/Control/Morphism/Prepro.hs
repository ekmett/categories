{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Morphism.Prepro
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
-- 
-- See Maarten Fokkinga''s PhD Dissertation for cascade and prepro.
-- g_prepro is an obvious generalization. The prepro variants of other
-- morphisms are distributed through the corresponding files.
----------------------------------------------------------------------------
module Control.Morphism.Prepro 
	( prepro, g_prepro, cascade, biprepro, g_biprepro
	) where

import Control.Comonad
import Control.Category.Hask
import Control.Functor
import Control.Functor.Pointed
import Control.Functor.Algebra 
import Control.Functor.Extras
import Control.Functor.Fix
import Control.Monad.Identity
import Control.Morphism.Cata

-- | @cascade f . map f = map f . cascade f@
-- | @cascade f = biprepro InB (first f)@
-- | @cascade f = x where x = InB . bimap id (x . fmap f) . outB@
-- | @cascade f = x where x = InB . bimap id (fmap f . x) . outB@
cascade :: Bifunctor s Hask Hask Hask => (a -> a) -> Fix s a -> Fix s a 
-- cascade f = biprepro InB (first f)
cascade f = x where x = InB . bimap id (x . fmap f) . outB 
-- equivalently: @cascade f = InB . bimap id (fmap f . cascade f) . outB@

-- | Fokkinga's Prepromorphism
prepro :: Functor f => Algebra f c -> (f :~> f) -> FixF f -> c
prepro f e = x where x = f . fmap (x . cata (InF . e)) . outF

-- | Generalized prepromorphisms, parameterized by a comonad
-- This is used to generate most of the specialized prepromorphisms in other modules.
-- You can use the distributive law combinators to build up analogues of other recursion 
-- schemes.
g_prepro :: (Functor f, Comonad w) => Dist f w -> GAlgebra f w a -> (f :~> f) -> FixF f -> a
g_prepro k g e = extract . c where c = liftW g . k . fmap (duplicate . c . cata (InF . e)) . outF

-- | Prepromorphisms for bifunctors
biprepro :: Bifunctor f Hask Hask Hask => Algebra (f a) c -> (f a :~> f a) -> Fix f a -> c
biprepro f e = x where x = f . bimap id (x . bicata (InB . e)) . outB

-- | Generalized bifunctor prepromorphism, parameterized by a comonad
g_biprepro :: (Bifunctor f Hask Hask Hask, Comonad w) => Dist (f a) w -> GAlgebra (f a) w c -> (f a :~> f a) -> Fix f a -> c
g_biprepro k g e = extract . c where c = liftW g . k . bimap id (duplicate . c . bicata (InB . e)) . outB
