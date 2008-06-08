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
module Control.Morphism.Histo 
	( distHisto
	, histo, g_histo
	, prepro_histo, g_prepro_histo
	) where

import Control.Functor.Algebra
import Control.Functor.Extras
import Control.Functor.Fix
import Control.Comonad
import Control.Comonad.Cofree
import Control.Morphism.Cata
import Control.Morphism.Prepro

distHisto :: (RunComonadCofree h w, Functor f) => Dist f h -> Dist f w
distHisto k = anaCofree (fmap extract) (k . fmap outCofree)

histo :: (RunComonadCofree f w) => GAlgebra f w a -> FixF f -> a
histo = g_cata (distHisto id)

g_histo :: (RunComonadCofree h w, Functor f) => Dist f h -> GAlgebra f w a -> FixF f -> a
g_histo k = g_cata (distHisto k)

-- A histomorphic prepromorphism
prepro_histo :: (RunComonadCofree f w) => GAlgebra f w a -> (f :~> f) -> FixF f -> a
prepro_histo = g_prepro (distHisto id)

-- A generalized histomorphic prepromorphism
g_prepro_histo :: (RunComonadCofree h w, Functor f) => Dist f h -> GAlgebra f w a -> (f :~> f) -> FixF f -> a
g_prepro_histo k = g_prepro (distHisto k)
