{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Ideal
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Monad.Ideal
	( 
	-- * Ideal Monads
	  MonadIdeal(..)
	, Ideal
	, ideal
	, destroyIdeal
	-- * Mutual recursion for (co)ideal (co)monad (co)products
	, Mutual(..)
	-- * Ideal Monad Coproduct
	, (:+)
	) where

import Control.Functor.Internal.Ideal
