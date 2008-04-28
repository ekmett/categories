{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Bifunctor.Instances
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Bifunctor.Instances where

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

instance Bifunctor Either where
	bimap = (+++)

instance Associative Either where
	associate (Left (Left a)) = Left a
	associate (Left (Right b)) = Right (Left b)
	associate (Right c) = Right (Right c)

instance Coassociative Either where
	coassociate (Left a) = Left (Left a)
	coassociate (Right (Left b)) = Left (Right b)
	coassociate (Right (Right c)) = Right c

instance Braided Either where
	braid (Left a) = Right a
	braid (Right b) = Left b

instance Symmetric Either

-- Either is NOT Comonoidal! bottom inhabits every type
