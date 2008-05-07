{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Either
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Incompatible with Control.Monad.Error, but removes the Error restriction
-- that prevents a natural encoding of Apomorphisms. This module is 
-- therefore incompatible with Control.Monad.Error
----------------------------------------------------------------------------
module Control.Monad.Either 
	( Either(..)
	, EitherT(..)
	) where

import Data.Either
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Functor.Pointed

newtype EitherT a m b = EitherT { runEitherT :: m (Either a b) }

{-
instance Functor (Either e) where
	fmap _ (Left a) = Left a
	fmap f (Right a) = Right (f a)

instance Pointed (Either e) where
	point = Right
-}

instance Applicative (Either e) where
	pure = Right
	(<*>) = ap

instance Monad (Either e) where
        return = Right
        Right m >>= k = k m
        Left e  >>= _ = Left e

instance MonadFix (Either e) where
	mfix f = let 
		a = f $ case a of
			Right r -> r
			_ -> error "empty mfix argument"
		in a

instance Functor f => Functor (EitherT a f) where
        fmap f = EitherT . fmap (fmap f) . runEitherT

instance Pointed f => Pointed (EitherT a f) where
	point = EitherT . point . Right

instance Monad m => Monad (EitherT a m) where
        return = EitherT . return . return
        m >>= k  = EitherT $ do
                a <- runEitherT m
                case a of
                	Left  l -> return (Left l)
                	Right r -> runEitherT (k r)

instance MonadFix m => MonadFix (EitherT a m) where
	mfix f = EitherT $ mfix $ \a -> runEitherT $ f $ case a of
        	Right r -> r
        	_       -> error "empty mfix argument"	
