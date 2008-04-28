{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.State
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs)
--
----------------------------------------------------------------------------
module Control.Comonad.State where

import Control.Arrow ((&&&), first)
import Control.Comonad
import Control.Comonad.State.Class

data StateC s a = StateC (s -> a) s
runStateC f s = (a b, b) where
	StateC a b = f (StateC id s)

instance ComonadState s (StateC s) where
	getC (StateC f s) = StateC id s
	putC s (StateC f _) = StateC f s

modifyC :: (s -> s) -> StateC s a -> StateC s a
modifyC m (StateC f s) = StateC f (m s)

instance Functor (StateC r) where
	fmap f (StateC f' s) = StateC (f . f') s

instance Comonad (StateC s) where
	extract (StateC f a) = f a
	extend k (StateC f v) = StateC (\v -> k (StateC f v)) v

newtype StateCT s w a = StateCT { runStateCT :: w (s -> a, s) }

instance Comonad w => ComonadState s (StateCT s w) where
	-- getC = uncurry id . extract . runStateCT
	getC = undefined
	putC = undefined

instance Functor f => Functor (StateCT b f) where
        fmap f = StateCT . fmap (first (f .)) . runStateCT

instance Comonad w => Comonad (StateCT b w) where
        extract = uncurry id . extract . runStateCT
        duplicate = undefined -- StateCT . liftW (fst . extract &&& StateCT) . duplicate . runStateCT
