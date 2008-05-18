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
	, PFree
	, Free
	, inFree
	, runFree
	, cataFree
	, free
	) where

import Prelude hiding ((.),id)
import Control.Category
import Control.Category.Cartesian
import Control.Functor
import Control.Functor.Combinators.Biff
import Control.Functor.Fix
import Control.Monad.Parameterized
import Control.Monad.Identity


type Free f a = Fix (PFree f) a

inFree :: f (Free f a) -> Free f a
inFree = InB . Biff . Right

runFree :: Free f a -> Either a (f (Free f a))
runFree = first runIdentity . runBiff . outB

cataFree :: Functor f => (c -> a) -> (f a -> a) -> Free f c -> a
cataFree l r = (l . runIdentity ||| r . fmap (cataFree l r)) . runBiff . outB

free :: Either a (f (Free f a)) -> Free f a
free = InB . Biff . first Identity
