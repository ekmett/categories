-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Indexed.State
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
--
----------------------------------------------------------------------------
module Control.Monad.Indexed.State where

import Control.Comonad
import Control.Arrow
import Control.Functor.Extras
import Control.Functor.HigherOrder
import Control.Functor.Indexed
import Control.Monad
import Control.Monad.Indexed

newtype IxState i j a = IxState { runIxState :: i -> (a, j) }

instance IxFunctor IxState where
	imap f (IxState m) = IxState (first f . m)

instance IxMonad IxState where
	ireturn = IxState . (,)
	ibind f (IxState m) = IxState $ \s1 -> let (a,s2) = m s1 in runIxState (f a) s2 
