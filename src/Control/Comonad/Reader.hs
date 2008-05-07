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
module Control.Comonad.Reader 
	( module Control.Bifunctor
	, module Control.Comonad
	, ReaderC(..)
	, runReaderC
	, ReaderCT(..)
	, ComonadReader(..)
	) where

import Control.Arrow ((&&&))
import Control.Bifunctor
import Control.Comonad
import Control.Monad.Instances

class Comonad w => ComonadReader r w | w -> r where
        askC :: w a -> r

data ReaderC r a = ReaderC r a 

runReaderC :: ReaderC r a -> (r, a)
runReaderC (ReaderC r a) = (r,a)

instance ComonadReader r (ReaderC r) where
	askC (ReaderC r _) = r

instance Functor (ReaderC r) where
	fmap f = uncurry ReaderC . second f . runReaderC

instance Copointed (ReaderC r) where
	extract (ReaderC _ a) = a

instance Comonad (ReaderC r) where
	duplicate (ReaderC e a) = ReaderC e (ReaderC e a)

instance Bifunctor ReaderC where
	bimap f g = uncurry ReaderC . bimap f g . runReaderC


newtype ReaderCT w r a = ReaderCT { runReaderCT :: w (r, a) }

instance Comonad w => ComonadReader r (ReaderCT w r) where
	askC = fst . extract . runReaderCT

instance Functor f => Functor (ReaderCT f b) where
        fmap f = ReaderCT . fmap (fmap f) . runReaderCT

instance Copointed w => Copointed (ReaderCT w b) where
        extract = snd . extract . runReaderCT

instance Comonad w => Comonad (ReaderCT w b) where
        duplicate = ReaderCT . liftW (fst . extract &&& ReaderCT) . duplicate . runReaderCT

instance Functor f => Bifunctor (ReaderCT f) where
	bimap f g = ReaderCT . fmap (bimap f g) . runReaderCT


instance ComonadReader e ((,)e) where
        askC = fst
