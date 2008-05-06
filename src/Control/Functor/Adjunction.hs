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

module Control.Functor.Adjunction where

import Control.Comonad
import Control.Functor.Composition
import Control.Functor.Composition.Class
import Control.Functor.Exponential
import Control.Functor.Full
import Control.Functor.Pointed
import Control.Monad

-- | An 'Adjunction' formed by the 'Functor' f and 'Functor' g. 

-- Minimal definition:

-- 1. @leftAdjunct@ and @rightAdjunct@ 

-- 2. @unit@ and @counit@

class (Functor f, Functor g) => Adjunction f g where
	unit   :: a -> g (f a)
	counit :: f (g a) -> a
	leftAdjunct  :: (f a -> b) -> a -> g b
	rightAdjunct :: (a -> g b) -> f a -> b

	unit = leftAdjunct id
	counit = rightAdjunct id
	leftAdjunct f = fmap f . unit
	rightAdjunct f = counit . fmap f




-- adjunction-oriented composition
newtype ACompF f g a = ACompF (CompF f g a) deriving (Functor, ExpFunctor, Full, Composition)

instance Adjunction f g => Pointed (ACompF g f) where
        point = compose . unit

instance Adjunction f g => Copointed (ACompF f g) where
        copoint = counit . decompose

instance Adjunction f g => Monad (ACompF g f) where
        return = point
        m >>= f = compose . fmap (rightAdjunct (decompose . f)) $ decompose m

instance Adjunction f g => Comonad (ACompF f g) where
        extract = copoint
        extend f = compose . fmap (leftAdjunct (f . compose)) . decompose
