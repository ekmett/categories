{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Bifunctor.Either
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Bifunctor.Either where

import Control.Bifunctor
import Control.Bifunctor.Associative
import Control.Bifunctor.Monoidal
import Control.Bifunctor.Braided
import Data.Void
import Control.Arrow ((***), (+++))

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
