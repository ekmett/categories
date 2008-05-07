{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Free
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Monad.Free 
	( module Control.Monad.Parameterized
	, module Control.Monad.Identity
	, FreeB
	, Free
	, inFree
	, runFree
	, cataFree
	, free
	) where

import Control.Arrow ((|||))
import Control.Bifunctor.Biff
import Control.Bifunctor.Fix
import Control.Monad.Parameterized
import Control.Monad.Identity

type FreeB f a b = BiffB Either Identity f a b
type Free f a = FixB (BiffB Either Identity f) a

inFree :: f (Free f a) -> Free f a
inFree = InB . BiffB . Right

runFree :: Free f a -> Either a (f (Free f a))
runFree = first runIdentity . runBiffB . outB

cataFree :: Functor f => (c -> a) -> (f a -> a) -> Free f c -> a
cataFree l r = (l . runIdentity ||| r . fmap (cataFree l r)) . runBiffB . outB

free :: Either a (f (Free f a)) -> Free f a
free = InB . BiffB . first Identity
