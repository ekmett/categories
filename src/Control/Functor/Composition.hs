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
-- Generalized functor composition.
-- Since we have many reasons for which you might want to compose a functor, and many 
-- expected results. i.e. monads via adjunctions, monads via composition with a pointed
-- endofunctor, etc. we have to make multiple composition operators.
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
import Control.Functor.HigherOrder
import Control.Category.Hask
import Control.Category.Braided

class Composition o where
	decompose  :: (f `o` g) x -> f (g x)
	compose    :: f (g x) -> (f `o` g) x

-- | Basic functor composition
newtype CompF f g a = CompF { runCompF :: f (g a) }

instance Composition CompF where
	compose = CompF
	decompose = runCompF

instance Functor f => HFunctor (CompF f) where
	ffmap = fmap
	hfmap f = compose . fmap f . decompose

-- | An infix alias for functor composition
type (:.:) = CompF

-- common functor composition traits
instance (Functor f, Functor g) => Functor (CompF f g) where
	fmap f = compose . fmap (fmap f) . decompose

instance (ExpFunctor f, ExpFunctor g) => ExpFunctor (CompF f g) where
        xmap f g = compose . xmap (xmap f g) (xmap g f) . decompose

instance (Full f, Full g) => Full (CompF f g) where
        premap f = premap . premap $ decompose . f . compose

-- | The only reason the compositions are all the same is for type inference. This can be liberalized.
associateComposition :: (Functor f, Composition c) => c (c f g) h a -> c f (c g h) a
associateComposition = compose . fmap compose . decompose . decompose

coassociateComposition :: (Functor f, Composition c) => c f (c g h) a -> c (c f g) h a
coassociateComposition = compose . compose . fmap decompose . decompose


-- | Bifunctor composition
newtype Comp p f g a b = Comp { runComp :: p (f a b) (g a b) }
-- | Bifunctor coproduct
type (:++:) = Comp Either
-- | Bifunctor product
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
