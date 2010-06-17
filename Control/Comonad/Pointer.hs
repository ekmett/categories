{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Pointer
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- SIGFPE (Dan Piponi)'s Pointer Comonad
----------------------------------------------------------------------------
module Control.Comonad.Pointer 
	( module Control.Comonad
	, Pointer(..)
	, distPointer
	) where

import Control.Functor.Extras
import Data.Array
import Control.Comonad

data Pointer i a = Pointer { index :: i, array :: Array i a } deriving (Show,Read)

instance Ix i => Functor (Pointer i) where
	fmap f (Pointer i a) = Pointer i (fmap f a)

instance Ix i => Copointed (Pointer i) where
	extract (Pointer i a) = a ! i

instance Ix i => Comonad (Pointer i) where
	extend f (Pointer i a) = Pointer i . listArray bds $ fmap (f . flip Pointer a) (range bds) where
		bds = bounds a

distPointer :: (Monad m, Ix i) => Dist (Pointer i) m 
distPointer (Pointer i ma) = do
	let bds = bounds ma
	a <- sequence (elems ma)
	return $ Pointer i (listArray bds a)
