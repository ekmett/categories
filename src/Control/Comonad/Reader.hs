{-# OPTIONS_GHC -fglasgow-exts #-}
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

import Control.Bifunctor
import Control.Bifunctor.Pair
import Control.Monad.Instances -- for Functor ((,)e)
import Control.Comonad.Reader.Class
import Control.Comonad
import Control.Arrow ((&&&))

data ReaderC r a = ReaderC r a 
runReaderC (ReaderC r a) = (r,a)

instance ComonadReader r (ReaderC r) where
	askC (ReaderC r _) = r

instance Functor (ReaderC r) where
	fmap f = uncurry ReaderC . second f . runReaderC

instance Comonad (ReaderC r) where
	extract (ReaderC _ a) = a
	duplicate (ReaderC e a) = ReaderC e (ReaderC e a)

instance Bifunctor ReaderC where
	bimap f g = uncurry ReaderC . bimap f g . runReaderC


newtype ReaderCT w r a = ReaderCT { runReaderCT :: w (r, a) }

instance Comonad w => ComonadReader r (ReaderCT w r) where
	askC = fst . extract . runReaderCT

instance Functor f => Functor (ReaderCT f b) where
        fmap f = ReaderCT . fmap (fmap f) . runReaderCT

instance Comonad w => Comonad (ReaderCT w b) where
        extract = snd . extract . runReaderCT
        duplicate = ReaderCT . liftW (fst . extract &&& ReaderCT) . duplicate . runReaderCT

instance Functor f => Bifunctor (ReaderCT f) where
	bimap f g = ReaderCT . fmap (bimap f g) . runReaderCT


instance Comonad ((,)e) where
        extract = snd
        duplicate ~(e,a) = (e,(e,a))

instance ComonadReader e ((,)e) where
        askC = fst

-- instance Functor ((,)e) where
--        fmap f = second f 

-- instance Bifunctor (,) where
--        bimap f g = uncurry ReaderC . bimap f g . runReaderC

