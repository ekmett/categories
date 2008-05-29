{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Functor.Lambek
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
-- 
----------------------------------------------------------------------------
module Control.Functor.Lambek
	( 
	-- * Lambek's Lemma
	  lambek
	, hlambek
	, colambek
	, hcolambek
	) where

import Control.Functor.Algebra 
import Control.Functor.Extras
import Control.Functor.Fix
import Control.Functor.HigherOrder
import Control.Morphism.Cata
import Control.Morphism.Ana

-- Lambek's lemma
lambek :: Functor f => Algebra f (FixF f) -> Coalgebra f (FixF f)
lambek inF = cata (fmap inF)

hlambek :: HFunctor f => HAlgebra f (FixH f) -> HCoalgebra f (FixH f)
hlambek inH = hcata (hfmap inH)

colambek :: Functor f => Coalgebra f (FixF f) -> Algebra f (FixF f)
colambek out = ana (fmap out)

hcolambek :: HFunctor f => HCoalgebra f (FixH f) -> HAlgebra f (FixH f)
hcolambek out = hana (hfmap out)

