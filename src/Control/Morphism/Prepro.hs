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
-- g_prepro is an obvious generalization.
----------------------------------------------------------------------------
module Control.Morphism.Prepro 
	( prepro, g_prepro, cascade
	) where

import Control.Comonad
import Control.Category.Hask
import Control.Functor
import Control.Functor.Pointed
import Control.Functor.Algebra 
import Control.Functor.Extras
import Control.Functor.Fix
-- import Control.Functor.HigherOrder
import Control.Monad.Identity
import Control.Morphism.Cata

-- | @cascade f . map f = map f . cascade f@
cascade :: Bifunctor s Hask Hask Hask => (a -> a) -> Fix s a -> Fix s a 
cascade f = InB . bimap id (cascade f . fmap f) . outB 
-- equivalently:
-- cascade f = InB . bimap id (fmap f . cascade f) . outB 

prepro :: Functor f => (f c -> c) -> (f :~> f) -> FixF f -> c
prepro f e = x where x = f . fmap (x . cata (InF . e)) . outF

-- | Generalized prepromorphisms
g_prepro :: (Functor f, Comonad w) => Dist f w -> GAlgebra f w a -> (f :~> f) -> FixF f -> a
g_prepro k g e = extract . c where c = liftW g . k . fmap (duplicate . c . cata (InF . e)) . outF

--repro :: Functor f => (f b -> b) -> (f :~> f) -> (f :~> f) -> (a -> f a) -> a -> b
--repro f fe ge g = x where x = f . fmap (ana (fe . outF) . x . cata (InF . ge)) . g
