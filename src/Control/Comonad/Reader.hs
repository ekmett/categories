{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Reader
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Comonad.Reader where

import Control.Comonad.Reader.Class
import Control.Comonad
import Control.Arrow ((&&&), second)

data ReaderC r a = ReaderC r a 
runReaderC (ReaderC r a) = (r,a)

instance ComonadReader r (ReaderC r) where
	askC (ReaderC r _) = r

instance Functor (ReaderC r) where
	fmap f = uncurry ReaderC . second f . runReaderC

instance Comonad (ReaderC r) where
	extract (ReaderC _ a) = a
	duplicate (ReaderC e a) = ReaderC e (ReaderC e a)



newtype ReaderCT r w a = ReaderCT { runReaderCT :: w (r, a) }

instance Comonad w => ComonadReader r (ReaderCT r w) where
	askC = fst . extract . runReaderCT

instance Functor f => Functor (ReaderCT b f) where
        fmap f = ReaderCT . fmap (fmap f) . runReaderCT

instance Comonad w => Comonad (ReaderCT b w) where
        extract = snd . extract . runReaderCT
        duplicate = ReaderCT . liftW (fst . extract &&& ReaderCT) . duplicate . runReaderCT
