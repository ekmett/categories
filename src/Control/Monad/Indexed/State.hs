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

import Control.Arrow ((***))
import Control.Bifunctor (Bifunctor(bimap), first)
import Control.Monad
import Control.Monad.Indexed

class IxMonad m => IxMonadState m where
	iget :: m i i i
	iput :: j -> m i j ()

imodify :: IxMonadState m => (i -> j) -> m i j ()
imodify f = iget >>>= iput . f

igets :: IxMonadState m => (i -> a) -> m i i a
igets f = iget >>>= ireturn . f
	
newtype IxState i j a = IxState { runIxState :: i -> (a, j) }

instance Functor (IxState i j) where
	fmap = imap

instance IxFunctor IxState where
	imap f m = IxState (first f . runIxState m)

instance IxPointed IxState where
	ireturn = IxState . (,)

instance IxApplicative IxState where
	iap = iapIxMonad

instance IxMonad IxState where
	ibind f m = IxState $ \s1 -> let (a,s2) = runIxState m s1 in runIxState (f a) s2 

instance IxMonadState IxState where
	iget = IxState (\x -> (x,x))
	iput x = IxState (\_ -> ((),x))

instance Bifunctor (IxState i) where 
	bimap f g m = IxState $ (g *** f) . runIxState m

newtype IxStateT m i j a = IxStateT { runIxStateT :: i -> m (a, j) }

instance Monad m => Functor (IxStateT m i j) where
	fmap = imap

instance Monad m => IxFunctor (IxStateT m) where
	imap f m = IxStateT $ \s -> runIxStateT m s >>= \(x,s') -> return (f x, s')

instance Monad m => IxPointed (IxStateT m) where
    	ireturn a = IxStateT $ \s -> return (a, s)

instance Monad m => IxApplicative (IxStateT m) where
   	iap = iapIxMonad 

instance Monad m => IxMonad (IxStateT m) where
    	ibind k m = IxStateT $ \s -> runIxStateT m s >>= \ ~(a, s') -> runIxStateT (k a) s'

instance Monad m => Bifunctor (IxStateT m i) where
	bimap f g m = IxStateT $ liftM (g *** f) . runIxStateT m

instance Monad m => IxMonadState (IxStateT m) where
	iget   = IxStateT $ \s -> return (s, s)
	iput s = IxStateT $ \_ -> return ((), s)

{-
instance MonadPlus m => IxMonadPlus (IxStateT m) where
    izero       = IxStateT $ \_ -> mzero
    m `iplus` n = IxStateT $ \s -> runIxStateT m s `mplus` runIxStateT n s

instance MonadFix m => IxMonadFix (IxStateT m) where
    ifix f = StateT $ \s -> mfix $ \ ~(a, _) -> runStateT (f a) s
-}
