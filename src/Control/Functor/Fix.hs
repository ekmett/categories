-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Functor.Fix
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
--
----------------------------------------------------------------------------
module Control.Functor.Fix 
	( Fix(InF,outF)
	, outM, inW
	, FixB(InB,outB)
	, paugment, pcoaugment
	) where

import Control.Monad
import Control.Comonad
import Control.Functor.Algebra
import Control.Monad.Parameterized
import Control.Comonad.Parameterized
import Control.Comonad
import Control.Category.Hask
import Control.Morphism.Hylo

newtype Fix f = InF { outF :: f (Fix f) }

outM :: (Functor f, Monad m) => CoAlgM f m (Fix f)
outM = liftCoAlg outF

inW :: (Functor f, Comonad w) => AlgW f w (Fix f)
inW = liftAlg InF

-- * Fixpoint of a bifunctor

newtype FixB s a = InB { outB :: s a (FixB s a) }

instance Bifunctor s Hask Hask Hask => Functor (FixB s) where
        fmap f = InB . bimap f (fmap f) . outB

instance (Bifunctor f Hask Hask Hask, PCopointed f) => Copointed (FixB f) where
        extract = pextract . outB

instance (Bifunctor f Hask Hask Hask, PPointed f) => Pointed (FixB f) where
        point = InB . preturn

instance (Bifunctor f Hask Hask Hask, PComonad f) => Comonad (FixB f) where
        extend k w = pcoaugment (\g -> bihylo InB id g w) k

instance (Bifunctor f Hask Hask Hask, PMonad f) => Monad (FixB f) where
        return = InB . preturn
        m >>= k = paugment (\f -> bihylo f id outB m) k

paugment :: PMonad f => (forall c. (f a c -> c) -> c) -> (a -> FixB f b) -> FixB f b
paugment g k = g (InB . pbind (outB . k))

pcoaugment :: PComonad f => ((FixB f a -> f b (FixB f a)) -> FixB f b) -> (FixB f a -> b) -> FixB f b
pcoaugment g k = g (pextend (k . InB) . outB)
