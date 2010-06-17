{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Morphism.Meta.Gibbons
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
--
-- A very basic Jeremy Gibbons metamorphism, without all 
-- the productive stream stuff. See:
-- <http://www.comlab.ox.ac.uk/oucl/work/jeremy.gibbons/publications/metamorphisms-scp.pdf>
-- TODO: Add some support for spigot algorithms over streams/lists.
----------------------------------------------------------------------------
module Control.Morphism.Meta.Gibbons 
	( meta
	, g_meta
	) where

import Control.Functor.Algebra
import Control.Functor.Extras
import Control.Functor.Fix
import Control.Comonad
import Control.Monad.Identity
import Control.Morphism.Ana
import Control.Morphism.Cata

-- Jeremy Gibbons' metamorphism
meta :: (Functor f, Functor g) => 
	  Coalgebra f b -> (a -> b) -> Algebra g a -> FixF g -> FixF f
meta f e g = ana f . e . cata g

-- | Generalized Jeremy Gibbons metamorphism
g_meta :: (Monad m, Functor f, Comonad w, Functor g) => 
	  Dist m f -> Dist g w -> GCoalgebra f m b -> (a -> b) -> GAlgebra g w a -> FixF g -> FixF f
g_meta m w f e g = g_ana m f . e . g_cata w g

