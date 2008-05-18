{-# OPTIONS_GHC -fglasgow-exts #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Monad.Indexed.Cont
-- Copyright 	: 2008 Edward Kmett, Dan Doel
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: Rank-2 Types
--
-------------------------------------------------------------------------------------------

module Control.Monad.Indexed.Cont where

import Control.Applicative
import Control.Functor.Pointed
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Indexed
import Control.Monad.Cont.Class
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Control.Monad.Indexed.Trans

class IxMonad m => IxMonadCont m where
	reset :: m a o o -> m r r a
	shift :: (forall i. (a -> m i i o) -> m r j j) -> m r o a

newtype IxContT m r o a = IxContT { runIxContT :: (a -> m o) -> m r }

runIxContT_ :: Monad m => IxContT m r a a -> m r 
runIxContT_ m = runIxContT m return

instance IxFunctor (IxContT m) where
	imap f m = IxContT $ \c -> runIxContT m (c . f)

instance IxPointed (IxContT m) where
	ireturn a = IxContT ($a)

instance Monad m => IxApplicative (IxContT m) where
	iap = iapIxMonad

instance Monad m => IxMonad (IxContT m) where
	ibind f c = IxContT $ \k -> runIxContT c $ \a -> runIxContT (f a) k

instance Monad m => IxMonadCont (IxContT m) where
	reset e = IxContT $ \k -> runIxContT e return >>= k
	shift e = IxContT $ \k -> e (\a -> IxContT (\k' -> k a >>= k')) `runIxContT` return

instance Monad m => Functor (IxContT m i j) where
	fmap = imap

instance Monad m => Pointed (IxContT m i i) where
	point = ireturn

instance Monad m => Applicative (IxContT m i i) where
	pure = ireturn
	(<*>) = iap

instance Monad m => Monad (IxContT m i i) where
	return = ireturn
	m >>= k = ibind k m

instance Monad m => MonadCont (IxContT m i i) where 
	callCC = undefined -- shift (\k -> f k >>= k)

instance IxMonadTrans IxContT where
	ilift m = IxContT (m >>=)

instance MonadReader e m => MonadReader e (IxContT m i i) where
	ask = ilift ask
	local f m = IxContT $ \c -> do
		r <- ask
		local f (runIxContT m (local (const r) . c))

instance MonadState e m => MonadState e (IxContT m i i) where
	get = ilift get
	put = ilift . put

instance MonadIO m => MonadIO (IxContT m i i) where
	liftIO = ilift . liftIO 

newtype IxCont r o a = IxCont (IxContT Identity r o a) 
	deriving (IxFunctor, IxPointed, IxApplicative, IxMonad, IxMonadCont)


runIxCont :: IxCont r o a -> (a -> o) -> r 
runIxCont (IxCont k) f = runIdentity $ runIxContT k (return . f)

runIxCont_ :: IxCont r a a -> r
runIxCont_ m = runIxCont m id

instance MonadCont (IxCont i i) where 
	callCC = undefined -- shift (\k -> f k >>= k)

instance Functor (IxCont i j) where
	fmap = imap

instance Pointed (IxCont i i) where
	point = ireturn

instance Applicative (IxCont i i) where
	pure = ireturn
	(<*>) = iap

instance Monad (IxCont i i) where
	return = ireturn
	m >>= k = ibind k m

