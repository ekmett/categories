-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Functor.Indexed
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Functor.Indexed where

import Control.Comonad
import Control.Arrow
import Control.Functor.Extras
import Control.Functor.HigherOrder
import Control.Monad

class IxFunctor f where
	imap :: (a -> b) -> f j k a -> f j k b

newtype LiftIx m i j a = LiftIx { lowerIx :: m a }

instance Functor f => IxFunctor (LiftIx f) where
        imap f = LiftIx . fmap f . lowerIx

newtype LowerIx m i a = LowerIx { liftIx :: m i i a } 

instance IxFunctor f => Functor (LowerIx f i) where
	fmap f = LowerIx . imap f . liftIx
