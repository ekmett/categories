{-# OPTIONS -fglasgow-exts #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Composition
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-- Generalized functor composeosition.
-------------------------------------------------------------------------------------------

module Control.Functor.Composition where

import Control.Functor.Composition.Class
import Control.Functor.Exponential
import Control.Functor.Full

newtype CompF f g a = CompF { runCompF :: f (g a) }

instance Composition CompF where
	compose = CompF
	decompose = runCompF

#ifndef __HADDOCK__
type (f :.: g) a = CompF f g a
#endif

-- common functor composition traits
instance (Functor f, Functor g) => Functor (CompF f g) where
	fmap f = compose . fmap (fmap f) . decompose

instance (ExpFunctor f, ExpFunctor g) => ExpFunctor (CompF f g) where
        xmap f g = compose . xmap (xmap f g) (xmap g f) . decompose

instance (Full f, Full g) => Full (CompF f g) where
        premap f = premap . premap $ decompose . f . compose

associateComp :: (Functor f, Composition c) => (c (c f g) h) a -> (c f (c g h)) a
associateComp = compose . fmap compose . decompose . decompose

coassociateComp :: (Functor f, Composition c) => (c f (c g h)) a -> (c (c f g) h) a
coassociateComp = compose . compose . fmap decompose . decompose
