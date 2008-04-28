{-# OPTIONS -fglasgow-exts #-}
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
-- that prevents a natural encoding of Apomorphisms
----------------------------------------------------------------------------
module Control.Monad.Either where

import Control.Monad
import Control.Arrow ((|||), (&&&), (+++), (***))

newtype EitherT a m b = EitherT { runEitherT :: m (Either a b) }

instance Monad (Either e) where
        return = Right
        Right m >>= k = k m
        Left e  >>= k = Left e

instance Functor f => Functor (EitherT b f) where
        fmap f = EitherT . fmap (fmap f) . runEitherT

instance Monad m => Monad (EitherT b m) where
        return = EitherT . return . return
        m >>= k  = EitherT $ do
                a <- runEitherT m
                case a of
                    Left  l -> return (Left l)
                    Right r -> runEitherT (k r)
