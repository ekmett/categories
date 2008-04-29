module Control.Bifunctor.Composition where

import Control.Comonad
import Control.Bifunctor
import Control.Bifunctor.Associative
import Control.Bifunctor.Braided
import Control.Bifunctor.Monoidal
import Control.Functor.Pointed
import Control.Functor.Exponential
import Control.Functor.Contravariant

newtype ArrowB f g a b = ArrowB { runArrowB :: f a b -> g a b }



newtype ConstB t a b = ConstB { runConstB :: t } 

instance Bifunctor (ConstB t) where
	bimap f g = ConstB . runConstB
instance Functor (ConstB t a) where
	fmap f = ConstB . runConstB




newtype FstB a b = FstB { runFstB :: a } 

instance Bifunctor FstB where
	bimap f g = FstB . f . runFstB 

instance Associative FstB where
	associate = FstB . runFstB . runFstB

instance Functor (FstB a) where
        fmap f (FstB a) = FstB a

instance ContravariantFunctor (FstB a) where
        contramap f (FstB a) = FstB a

instance ExpFunctor (FstB a) where
        xmap f g (FstB a) = FstB a


newtype SndB a b = SndB { runSndB :: b } 

instance Bifunctor SndB where
	bimap f g = SndB . g . runSndB 

-- instance Coassociative SndB where
--	coassociate = SndB . SndB . runSndB

-- as a functor its a family of identity functors with a type-level parameter (a)
instance Functor (SndB a) where
	fmap = bimap id

-- bifunctor composition

newtype CompB p f g a b = CompB { runCompB :: p (f a b) (g a b) }

instance (Bifunctor p, Bifunctor f, Bifunctor g) => Bifunctor (CompB p f g) where
	bimap f g = CompB . bimap (bimap f g) (bimap f g) . runCompB

liftCompB :: Bifunctor p => (f a b -> f c d) -> (g a b -> g c d) -> CompB p f g a b -> CompB p f g c d 
liftCompB f g = CompB . bimap f g . runCompB

instance (Bifunctor p, Braided f, Braided g) => Braided (CompB p f g) where
	braid = liftCompB braid braid

instance (Bifunctor p, Symmetric f, Symmetric g) => Symmetric (CompB p f g) 

instance (Bifunctor p, Bifunctor f, Bifunctor g) => Functor (CompB p f g a) where
	fmap = bimap id



newtype SwapB p a b = SwapB { runSwapB :: p b a } 

liftSwapB :: Bifunctor p => (p a b -> p c d) -> SwapB p b a -> SwapB p d c
liftSwapB f = SwapB . f . runSwapB

instance Bifunctor p => Bifunctor (SwapB p) where
	bimap f g = liftSwapB (bimap g f)

{-
instance Coassociative p => Associative (SwapB p) where
	associate = liftSwapB coassociate

instance Associative p => Coassociative (SwapB p) where
	coassociate = liftSwapB associate
-}

instance Braided p => Braided (SwapB p) where
	braid = liftSwapB braid

instance Symmetric p => Symmetric (SwapB p)

{-
instance HasIdentity p i => HasIdentity (SwapB p) i

instance Monoidal p i => Monoidal (SwapB p) i where
	idl = idr . runSwapB
	idr = idl . runSwapB

instance Comonoidal p i => Comonoidal (SwapB p) i where
	coidl = SwapB . coidr
	coidr = SwapB . coidl
-}

instance Bifunctor p => Functor (SwapB p a) where
	fmap = bimap id

-- a functor composed around a bifunctor

newtype FunctorB f p a b = FunctorB { runFunctorB :: f (p a b) } 

liftFunctorB :: Functor f => (p a b -> p c d) -> FunctorB f p a b -> FunctorB f p c d
liftFunctorB f = FunctorB . fmap f . runFunctorB

instance (Functor f, Bifunctor p) => Bifunctor (FunctorB f p) where
	bimap f g = liftFunctorB (bimap f g)

instance (Functor f, Braided p) => Braided (FunctorB f p) where
	braid = liftFunctorB braid

instance (Functor f, Symmetric p) => Symmetric (FunctorB f p) 

{-
instance (Functor f, Associative p) => Associative (FunctorB f p) where
	associate = FunctorB . fmap associate . runFunctorB 

instance (Functor f, Coassociative p) => Coassociative (FunctorB f p) where
	coassociate = FunctorB . fmap f . runFunctorB

instance (Functor f, HasIdentity p i) => HasIdentity (FunctorB f p) i


instance (Copointed f, Monoidal p i) => Monoidal (FunctorB f p) i where
	idr = idr . copoint . runFunctorB
	idl = idl . copoint . runFunctorB
	
instance (Pointed f, Comonoidal p i) => Comonoidal (FunctorB f p) i where
	coidr = FunctorB . point . coidr
	coidl = FunctorB . point . coidl
-}

instance (Functor f, Bifunctor p) => Functor (FunctorB f p a) where
	fmap = bimap id
