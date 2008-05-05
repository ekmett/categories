-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Indexed
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
--
----------------------------------------------------------------------------
module Control.Monad.Indexed where

import Control.Comonad
import Control.Arrow
import Control.Functor.Extras
import Control.Functor.HigherOrder
import Control.Functor.Indexed
import Control.Monad

class IxFunctor m => IxMonad m where
	ireturn :: a -> m i i a
	ibind :: (a -> m j k b) -> m i j a -> m i k b

ijoin :: IxMonad m => m i j (m j k a) -> m i k a 
ijoin = ibind id

infixl 1 >>>=

(>>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
m >>>= k = ibind k m 

instance (Functor m, Monad m) => IxMonad (LiftIx m) where
	ireturn = LiftIx . return
	ibind f m = LiftIx (lowerIx m >>= lowerIx . f)

instance (IxMonad m) => Monad (LowerIx m i) where
	return = LowerIx . ireturn
	m >>= f = LowerIx (liftIx m >>>= liftIx . f)
