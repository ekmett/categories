{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
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

module Control.Functor.Composition
	( CompF(..)
	, Composition(..)
	, associateComp
	, coassociateComp
	, (:.:)
	) where

import Control.Functor.Exponential
import Control.Functor.Full

class Composition c where
    decompose  :: c f g x -> f (g x)
    compose    :: f (g x) -> c f g x

newtype CompF f g a = CompF { runCompF :: f (g a) }

instance Composition CompF where
	compose = CompF
	decompose = runCompF

type (f :.: g) a = CompF f g a

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
