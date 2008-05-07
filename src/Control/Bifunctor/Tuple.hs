{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Bifunctor.Pair
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Bifunctor.Pair where

import Control.Bifunctor
import Control.Bifunctor.Associative
import Control.Bifunctor.Monoidal
import Control.Bifunctor.Braided
import Data.Void
import Control.Arrow ((***), (+++))

instance Bifunctor (,) where
        bimap = (***)

instance Associative (,) where
	associate ((a,b),c) = (a,(b,c))

instance Coassociative (,) where
	coassociate (a,(b,c)) = ((a,b),c)

instance HasIdentity (,) Void

instance Monoidal (,) Void where
	idl = snd
	idr = fst

instance Braided (,) where
	braid ~(a,b) = (b,a)

instance Symmetric (,)

