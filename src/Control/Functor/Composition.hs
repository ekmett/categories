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
	, associateComposition
	, coassociateComposition
	, (:.:)
	, Comp
	, (:++:)
	, (:**:)
	, liftComp
	) where

import Control.Functor
import Control.Functor.Exponential
import Control.Functor.Full
import Control.Category.Hask
import Control.Category.Braided

class Composition c where
	decompose  :: c f g x -> f (g x)
	compose    :: f (g x) -> c f g x

newtype CompF f g a = CompF { runCompF :: f (g a) }

instance Composition CompF where
	compose = CompF
	decompose = runCompF

type (:.:) = CompF

-- common functor composition traits
instance (Functor f, Functor g) => Functor (CompF f g) where
	fmap f = compose . fmap (fmap f) . decompose

instance (ExpFunctor f, ExpFunctor g) => ExpFunctor (CompF f g) where
        xmap f g = compose . xmap (xmap f g) (xmap g f) . decompose

instance (Full f, Full g) => Full (CompF f g) where
        premap f = premap . premap $ decompose . f . compose

associateComposition :: (Functor f, Composition c) => c (c f g) h a -> c f (c g h) a
associateComposition = compose . fmap compose . decompose . decompose

coassociateComposition :: (Functor f, Composition c) => c f (c g h) a -> c (c f g) h a
coassociateComposition = compose . compose . fmap decompose . decompose

newtype Comp p f g a b = Comp { runComp :: p (f a b) (g a b) }
type (:++:) = Comp Either
type (:**:) = Comp (,)

instance (Bifunctor p c d Hask, PFunctor f a c, PFunctor g a d) => PFunctor (Comp p f g) a Hask where
	first f = Comp . bimap (first f) (first f) . runComp

instance (Bifunctor p c d Hask, QFunctor f b c, QFunctor g b d) => QFunctor (Comp p f g) b Hask where
	second g = Comp . bimap (second g) (second g) . runComp

instance (Bifunctor p c d Hask, Bifunctor f a b c, Bifunctor g a b d) => Bifunctor (Comp p f g) a b Hask where
	bimap f g = Comp . bimap (bimap f g) (bimap f g) . runComp

liftComp :: Bifunctor p r s Hask => r (f a b) (f c d) -> s (g a b) (g c d) -> Comp p f g a b -> Comp p f g c d 
liftComp f g = Comp . bimap f g . runComp

instance (Bifunctor p Hask Hask Hask, Braided Hask f, Braided Hask g) => Braided Hask (Comp p f g) where
	braid = liftComp braid braid

instance (Bifunctor p Hask Hask Hask, Symmetric Hask f,  Symmetric Hask g) => Symmetric Hask (Comp p f g) 

instance (Bifunctor p Hask Hask Hask, Bifunctor f Hask Hask Hask, Bifunctor g Hask Hask Hask) => Functor (Comp p f g a) where
	fmap = bimap id
