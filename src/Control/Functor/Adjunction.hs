{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Adjunction
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (functional-dependencies)
--
-------------------------------------------------------------------------------------------

module Control.Functor.Adjunction 
	( Adjunction (unit, counit, leftAdjunct, rightAdjunct)
	, ACompF(ACompF)
	) where

import Control.Functor.Composition
import Control.Functor.Exponential
import Control.Functor.Full
import Control.Functor.HigherOrder
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Control.Comonad.Reader
import Control.Comonad.Context

-- | An 'Adjunction' formed by the 'Functor' f and 'Functor' g. 

-- Minimal definition:

-- 1. @leftAdjunct@ and @rightAdjunct@ 

-- 2. @unit@ and @counit@

class (Functor f, Functor g) => Adjunction f g | f -> g, g -> f where
	unit   :: a -> g (f a)
	counit :: f (g a) -> a
	leftAdjunct  :: (f a -> b) -> a -> g b
	rightAdjunct :: (a -> g b) -> f a -> b

	unit = leftAdjunct id
	counit = rightAdjunct id
	leftAdjunct f = fmap f . unit
	rightAdjunct f = counit . fmap f

instance (Adjunction f1 g1, Adjunction f2 g2) => Adjunction (CompF f2 f1) (CompF g1 g2) where
	counit = counit . fmap (counit . fmap decompose) . decompose
	unit = compose . fmap (fmap compose . unit) . unit

-- | Adjunction-oriented composition, yields monads and comonads from adjunctions
newtype ACompF f g a = ACompF (CompF f g a) deriving (Functor, ExpFunctor, Full, Composition, HFunctor)

instance Adjunction f g => Pointed (ACompF g f) where
        point = compose . unit

instance Adjunction f g => Copointed (ACompF f g) where
        extract = counit . decompose

instance Adjunction f g => Applicative (ACompF g f) where
	pure = point
	(<*>) = ap

instance Adjunction f g => Monad (ACompF g f) where
        return = point
        m >>= f = compose . fmap (rightAdjunct (decompose . f)) $ decompose m

instance Adjunction f g => Comonad (ACompF f g) where
        extend f = compose . fmap (leftAdjunct (f . compose)) . decompose

instance Adjunction ((,)e) ((->)e) where
	unit a e = (e,a)
	counit (x,f) = f x

instance Adjunction (Coreader e) (Reader e) where
	unit a = Reader (\e -> Coreader e a)
	counit (Coreader x f) = runReader f x

instance ComonadContext e ((,)e `ACompF` (->)e) where
	getC = fst . decompose
	modifyC f = uncurry (flip id . f) . decompose

instance MonadState e ((->)e `ACompF` (,)e) where
	get = compose $ \s -> (s,s)
	put s = compose $ const (s,())
