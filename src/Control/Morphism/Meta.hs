{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Morphism.Meta
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
--
-- Generalized metamorphisms 
----------------------------------------------------------------------------
module Control.Morphism.Meta where


import Control.Functor.Algebra
import Control.Functor.Extras
import Control.Functor.Fix
import Control.Comonad
import Control.Monad.Identity
import Control.Morphism.Ana
import Control.Morphism.Cata


meta :: (Functor f, Functor g) => 
	  CoAlg f b -> (a -> b) -> Alg g a -> Fix g -> Fix f
meta f e g = ana f . e . cata g

g_meta :: (Monad m, Functor f, Comonad w, Functor g) => 
	  Dist m f -> Dist g w -> CoAlgM f m b -> (a -> b) -> AlgW g w a -> Fix g -> Fix f
g_meta m w f e g = g_ana m f . e . g_cata w g

