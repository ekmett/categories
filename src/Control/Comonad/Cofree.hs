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
	, CofreeLike(unwrap)
	, Expensive(..)
	, coimprove, worsten
	) where

import Control.Arrow ((&&&))
import Control.Comonad
import Control.Functor.Fix
import Control.Functor.Combinators.Biff
import Control.Functor.KanExtension
import Control.Monad.Identity

type Cofree f = Fix (PCofree f)

outCofree :: Cofree f a -> f (Cofree f a)
outCofree = snd . runCofree

runCofree :: Cofree f a -> (a, f (Cofree f a))
runCofree = runPCofree . outB

anaCofree :: Functor f => (a -> c) -> (a -> f a) -> a -> Cofree f c
anaCofree h t = InB . Biff . (Identity . h &&& fmap (anaCofree h t) . t)

cofree :: a -> f (Cofree f a) -> Cofree f a 
cofree a as = InB $ Biff (Identity a,as)

class (Functor f, Comonad w) => CofreeLike f w | w -> f where
        unwrap :: w a -> f (w a)

instance Functor f => CofreeLike f (Cofree f) where
        unwrap = outCofree 

instance CofreeLike f w => CofreeLike f (Lan w w) where
        unwrap (Lan f c) = fmap (Lan f) (unwrap c)

data Expensive f a = forall w. CofreeLike f w => Expensive { runExpensive :: w a }

coimprove :: Functor f => Cofree f a -> Expensive f a
coimprove m = Expensive (coabs m)

worsten :: Functor f => (forall w. CofreeLike f w => w a) -> Cofree f a
worsten m = corep m 
