{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Morphism.Meta.Erwig
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
--
-- Martin Erwig's metamorphisms for indexed data types.
-- 
-- ADT fusion: @snd c . fst c == id  => erwig d id c . erwig c id d' = erwig d id d'@
-- 
-- FreeMeta: @l strict, snd c == snd c' == phi', fst d == fst d' == alpha, l . fst c = fst c' . fmap l, snd d' . rr = fmap r . snd d ==> l . (erwig d id c) = (erwig d' id c') . r@
----------------------------------------------------------------------------
module Control.Morphism.Meta.Erwig
	( meta
	) where

import Control.Functor.Algebra
import Control.Functor.Extras
import Control.Morphism.Hylo

-- | @meta d f c@ is Martin Erwig's metamorphism from @c@ to @d@
meta :: Functor h => Bialgebra m n b -> (h :~> m) -> Bialgebra f h a -> a -> b
meta d f c = hylo (fst d) f (snd c)

