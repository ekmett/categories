{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Morphism.Futu
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
-- 
-- Traditional operators, shown here to show how to roll your own
----------------------------------------------------------------------------
module Control.Morphism.Futu 
	( futu, g_futu
	, postpro_futu, g_postpro_futu
	, distFutu
	) where

import Control.Functor.Algebra
import Control.Functor.Extras
import Control.Functor.Fix
import Control.Monad.Free
import Control.Morphism.Ana
import Control.Morphism.Postpro

-- | Generalized from @futu :: Functor f => GCoalgebra f (Free f) a -> a -> FixF f@
futu :: (RunMonadFree f m) => GCoalgebra f m a -> a -> FixF f
futu = g_ana (distFutu id)

g_futu :: (Functor f, RunMonadFree h m) => Dist h f -> GCoalgebra f m a -> a -> FixF f
g_futu k = g_ana (distFutu k)

-- | A futumorphic postpromorphism
postpro_futu :: (RunMonadFree f m) => GCoalgebra f m a -> (f :~> f) -> a -> FixF f
postpro_futu = g_postpro (distFutu id)

-- | A generalized-futumorphic postpromorphism
g_postpro_futu :: (Functor f, RunMonadFree h m) => Dist h f -> GCoalgebra f m a -> (f :~> f) -> a -> FixF f
g_postpro_futu k = g_postpro (distFutu k)

-- | Turn a distributive law for a functor into a distributive law for the free monad of that functor.
-- This has been generalized to support generating distributive laws for a number of related free-monad-like
-- constructions such as the Codensity monad of the free monad of a functor.
distFutu :: (Functor f, RunMonadFree h m) => Dist h f -> Dist m f
distFutu k = cataFree (fmap return) (fmap inFree . k)
