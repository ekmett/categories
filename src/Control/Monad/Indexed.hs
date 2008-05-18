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
module Control.Monad.Indexed 
	( IxFunctor(..)
	, IxPointed(..)
	, IxApplicative(..)
	, IxMonad(..)
	, IxMonadZero(..)
	, IxMonadPlus(..)
	, ijoin, (>>>=), (=<<<)
	, iapIxMonad
	) where

import Control.Functor.Indexed

class IxApplicative m => IxMonad m where
	ibind :: (a -> m j k b) -> m i j a -> m i k b

ijoin :: IxMonad m => m i j (m j k a) -> m i k a 
ijoin = ibind id

infixr 1 =<<<
infixl 1 >>>=

(>>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
m >>>= k = ibind k m 

(=<<<) :: IxMonad m => (a -> m j k b) -> m i j a -> m i k b
(=<<<) = ibind

iapIxMonad :: IxMonad m => m i j (a -> b) -> m j k a -> m i k b
iapIxMonad f x = f >>>= \ f' -> x >>>= \x' -> ireturn (f' x')

class IxMonad m => IxMonadZero m where
	imzero :: m i j a

class IxMonadZero m => IxMonadPlus m where
	implus :: m i j a -> m i j a -> m i j a
