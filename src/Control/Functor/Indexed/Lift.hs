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
module Control.Functor.Indexed.Lift where

import Control.Functor.Indexed
import Control.Functor.Pointed.Indexed
import Control.Applicative.Indexed
import Control.Monad.Indexed
import Control.Comonad.Indexed

newtype LiftIx m i j a = LiftIx { lowerIx :: m a }

instance Functor f => IxFunctor (LiftIx f) where
        imap f = LiftIx . fmap f . lowerIx


instance Pointed m => IxPointed (LiftIx m) where
        ireturn = LiftIx . point

instance Copointed m => IxCopointed (LiftIx m) where
        iextract = extract . lowerIx

instance (Pointed m, Applicative m) => IxApplicative (LiftIx m) where
	iap f m = LiftIx (lowerIx f <*> lowerIx m)

instance (Pointed m, Applicative m, Monad m) => IxMonad (LiftIx m) where
	ibind f m = LiftIx (lowerIx m >>= lowerIx . f)

instance Comonad w => IxComonad (LiftIx w) where
	iextend f = LiftIx . extend (f . LiftIx) . lowerIx



newtype LowerIx m i a = LowerIx { liftIx :: m i i a } 

instance IxFunctor f => Functor (LowerIx f i) where
	fmap f = LowerIx . imap f . liftIx

instance IxPointed m => Pointed (LowerIx m i) where
	point = LowerIx . ireturn

instance IxCopointed m => Copointed (LowerIx m i) where
	extract = iextract . liftIx

instance IxApplicative m => Applicative (LowerIx m i) where
	pure = LowerIx . ireturn
	f <*> m = LowerIx (liftIx f `iap` liftIx m)

instance IxMonad m => Monad (LowerIx m i) where
	return = LowerIx . ireturn
	m >>= f = LowerIx (liftIx m >>>= liftIx . f)

instance IxComonad w => Comonad (LowerIx w i) where
	extend f = LowerIx . iextend (f . LowerIx) . liftIx
