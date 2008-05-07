-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Bifunctor.Fix
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-------------------------------------------------------------------------------------------
module Control.Bifunctor.Fix 
	( FixB(..)
	, paugment
	, pcoaugment
	) where

import Control.Monad.Parameterized
import Control.Comonad.Parameterized
import Control.Comonad
import Control.Morphism.Hylo

newtype FixB s a = InB { outB :: s a (FixB s a) }

instance Bifunctor s => Functor (FixB s) where
        fmap f = InB . bimap f (fmap f) . outB

instance PCopointed f => Copointed (FixB f) where
        extract = pextract . outB

instance PPointed f => Pointed (FixB f) where
        point = InB . preturn

instance PComonad f => Comonad (FixB f) where
        extend k w = pcoaugment (\g -> bihylo InB id g w) k

instance PMonad f => Monad (FixB f) where
        return = InB . preturn
        m >>= k = paugment (\f -> bihylo f id outB m) k

paugment :: PMonad f => (forall c. (f a c -> c) -> c) -> (a -> FixB f b) -> FixB f b
paugment g k = g (InB . pbind (outB . k))

pcoaugment :: PComonad f => ((FixB f a -> f b (FixB f a)) -> FixB f b) -> (FixB f a -> b) -> FixB f b
pcoaugment g k = g (pextend (k . InB) . outB)
