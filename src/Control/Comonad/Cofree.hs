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
module Control.Comonad.Cofree where

import Control.Arrow ((&&&))
import Control.Bifunctor
import Control.Bifunctor.Fix
import Control.Bifunctor.Composition
import Control.Bifunctor.Pair
import Control.Comonad
import Control.Comonad.Parameterized
import Control.Comonad.Parameterized.Class
import Control.Functor.Extras
import Control.Monad.Identity
import Control.Monad.Parameterized
import Control.Monad.Parameterized.Class

type CofreeB f a b = BiffB (,) Identity f a b
type Cofree f a = FixB (BiffB (,) Identity f) a

instance Functor f => PComonad (BiffB (,) Identity f) where
	pextract = runIdentity . fst . runBiffB
	pextend f = BiffB . (Identity . f &&& snd . runBiffB)

instance FunctorPlus f => PMonad (BiffB (,) Identity f) where
	preturn a = BiffB (Identity a,fzero)
	pbind k (BiffB ~(Identity a,as)) = BiffB (ib, fplus as bs) where BiffB (ib,bs) = k a 

outCofree :: Cofree f a -> f (Cofree f a)
outCofree = snd . runBiffB . outB

runCofree :: Cofree f a -> (a, f (Cofree f a))
runCofree = first runIdentity . runBiffB . outB

anaCofree :: Functor f => (a -> c) -> (a -> f a) -> a -> Cofree f c
anaCofree h t = InB . BiffB . (Identity . h &&& fmap (anaCofree h t) . t)

cofree :: a -> f (Cofree f a) -> Cofree f a 
cofree a as = InB $ BiffB (Identity a,as)

