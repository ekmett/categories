{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Functor.Combinators.Of
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Functor.Combinators.Of
	( Of(Of,runOf), liftOf
	) where

import Prelude hiding ((.),id)
import Control.Category
import Control.Category.Hask
import Control.Category.Braided
import Control.Functor
import Control.Functor.Pointed
-- import Control.Functor.Zip
-- import Control.Functor.Zap

newtype Of f p a b = Of { runOf :: f (p a b) }

liftOf :: Functor f => (p a b -> p c d) -> Of f p a b -> Of f p c d
liftOf f = Of . fmap f . runOf

instance (Functor f, PFunctor p Hask Hask) => PFunctor (f `Of` p) Hask Hask where
        first f = liftOf (first f)
instance (Functor f, QFunctor p Hask Hask) => QFunctor (f `Of` p) Hask Hask where
        second g = liftOf (second g)
instance (Functor f, Bifunctor p Hask Hask Hask) => Bifunctor (f `Of` p) Hask Hask Hask where
        bimap f g = liftOf (bimap f g)

instance (Functor f, Braided Hask p ) => Braided Hask (f `Of` p) where
        braid = liftOf braid

instance (Functor f, Symmetric Hask p) => Symmetric Hask (f `Of` p)

instance (Functor f, Functor (p a)) => Functor (Of f p a) where
        fmap f = Of . fmap (fmap f) . runOf

instance (Pointed f, PPointed p) => PPointed (f `Of` p) where
	preturn = Of . point . preturn

instance (Copointed f, PCopointed p) => PCopointed (f `Of` p) where
	pextract = pextract . extract . runOf

instance (Pointed f, Pointed (p a)) => Pointed (Of f p a) where
	point = Of . point . point

instance (Copointed f, Copointed (p a)) => Copointed (Of f p a) where
	extract = extract . extract . runOf

{-
instance (Zip f, Bizip p) => Bizip (f `Of` p) where
	bizipWith f g = Of . fzipWith (bizipWith f g) . runOf 

instance (Zip f, Zip (p a)) => Zip (Of f p a) where
	fzipWith f = Of . fzipWith (fzipWith f) . runOf

instance (Bizap p q, Zap f g) => Bizap (f `Of` p) (g `Of` q) where
	bizapWith f g = Of . zapWith (bizapWith f g) . runOf
-}
