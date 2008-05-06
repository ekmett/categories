{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Pointer
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs)
--
-- SIGFPE (Dan Piponi)'s Pointer Comonad
----------------------------------------------------------------------------
module Control.Comonad.Pointer where

import Control.Functor.Extras
import Control.Arrow ((&&&), first)
import Data.Array
import Data.Foldable
import Control.Monad
import Control.Comonad

data Pointer i a = Pointer { index :: i, array :: Array i a } deriving (Show,Read)

instance Ix i => Functor (Pointer i) where
	fmap f (Pointer i a) = Pointer i (fmap f a)

instance Ix i => Comonad (Pointer i) where
	extract (Pointer i a) = a ! i
	extend f (Pointer i a) = Pointer i . listArray bds $ fmap (f . flip Pointer a) (range bds) where
		bds = bounds a

distPointer :: (Monad m, Ix i) => Dist (Pointer i) m 
distPointer (Pointer i ma) = do
	let bds = bounds ma
	a <- sequence (elems ma)
	return $ Pointer i (listArray bds a)
