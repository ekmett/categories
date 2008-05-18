{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Cofree
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  rank-2 types 
--
----------------------------------------------------------------------------
module Control.Comonad.Cofree 
	( Cofree
	, outCofree, runCofree, anaCofree, cofree
	) where

import Control.Arrow ((&&&))
import Control.Functor.Fix
import Control.Functor.Combinators.Biff
import Control.Monad.Identity

type Cofree f = FixB (PCofree f)

outCofree :: Cofree f a -> f (Cofree f a)
outCofree = snd . runCofree

runCofree :: Cofree f a -> (a, f (Cofree f a))
runCofree = runPCofree . outB

anaCofree :: Functor f => (a -> c) -> (a -> f a) -> a -> Cofree f c
anaCofree h t = InB . Biff . (Identity . h &&& fmap (anaCofree h t) . t)

cofree :: a -> f (Cofree f a) -> Cofree f a 
cofree a as = InB $ Biff (Identity a,as)

