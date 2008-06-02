{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Coideal
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Comonad.Coideal
	( 
	-- * Coideal Comonads
	  ComonadCoideal(..)
	, Coideal
	, coideal
	, buildCoideal
	-- * Mutual recursion for (co)ideal (co)monad (co)products
	, Mutual(..)
	-- * Coideal Comonad Product
	, (:*)
	) where

import Control.Functor.Internal.Ideal
