{-# OPTIONS_GHC -fglasgow-exts #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Monad.Indexed.Cont
-- Copyright 	: 2008 Edward Kmett
--		  2008 Dan Doel
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-------------------------------------------------------------------------------------------

module Control.Monad.Indexed.Cont where

import Control.Monad.Identity
import Control.Functor.Indexed
import Control.Monad.Indexed


newtype IxContT m r o a = IxContT { runIxContT :: (a -> m o) -> m r }

runIxContT_ :: Monad m => IxContT m r a a -> m r 
runIxContT_ m = runIxContT m return

-- runIxContT :: Monad m => IxContT m r a a -> m r
-- runIxContT m = unIxContT m return

instance IxFunctor (IxContT m) where
	imap f m = IxContT $ \c -> runIxContT m (c . f)


instance Monad m => IxMonad (IxContT m) where
	ireturn a = IxContT ($a)
	ibind f c = IxContT $ \k -> runIxContT c $ \a -> runIxContT (f a) k

class IxMonad m => IxContMonad m where
	reset :: m a o o -> m r r a
	shift :: ((a -> m i i o) -> m r j j) -> m r o a


instance Monad m => IxContMonad (IxContT m) where
	reset e = IxContT $ \k -> runIxContT e return >>= k
	shift e = IxContT $ \k -> e (\a -> IxContT (\k' -> k a >>= k')) `runIxContT` return

newtype IxCont r o a = IxCont (IxContT Identity r o a) deriving (IxFunctor, IxMonad, IxContMonad)

runIxCont :: IxCont r o a -> (a -> o) -> r 
runIxCont (IxCont k) f = runIdentity $ runIxContT k (return . f)

runIxCont_ :: IxCont r a a -> r
runIxCont_ m = runIxCont m id
