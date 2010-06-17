{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Morphism.Span
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
--
-- Spans and Cospans
-- <http://en.wikipedia.org/wiki/Span_(category_theory)>
----------------------------------------------------------------------------
module Control.Morphism.Span 
	( Span(..)
	, Cospan(..)
	) where

newtype Span (~>) x y z = Span { runSpan :: (y ~> x, y ~> z) }
newtype Cospan (~>) x y z = Cospan { runCospan :: (x ~> y, z ~> y) }
