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
-- Since in Hask, Mu = Nu, we don't bother to distinguish them here
----------------------------------------------------------------------------
module Control.Functor.Fix 
	( 
	-- * Functor fixpoint
	  FixF(InF,outF)
	, outM, inW
	-- * Bifunctor fixpoint
	, Fix(InB,outB)
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

newtype FixF f = InF { outF :: f (FixF f) }

outM :: (Functor f, Monad m) => CoAlgM f m (FixF f)
outM = liftCoAlg outF

inW :: (Functor f, Comonad w) => AlgW f w (FixF f)
inW = liftAlg InF

-- * Fixpoint of a bifunctor

newtype Fix s a = InB { outB :: s a (Fix s a) }

instance Bifunctor s Hask Hask Hask => Functor (Fix s) where
        fmap f = InB . bimap f (fmap f) . outB

instance (Bifunctor f Hask Hask Hask, PCopointed f) => Copointed (Fix f) where
        extract = pextract . outB

instance (Bifunctor f Hask Hask Hask, PPointed f) => Pointed (Fix f) where
        point = InB . preturn

instance (Bifunctor f Hask Hask Hask, PComonad f) => Comonad (Fix f) where
        extend k w = pcoaugment (\g -> bihylo InB id g w) k

instance (Bifunctor f Hask Hask Hask, PMonad f) => Monad (Fix f) where
        return = InB . preturn
        m >>= k = paugment (\f -> bihylo f id outB m) k

paugment :: PMonad f => (forall c. (f a c -> c) -> c) -> (a -> Fix f b) -> Fix f b
paugment g k = g (InB . pbind (outB . k))

pcoaugment :: PComonad f => ((Fix f a -> f b (Fix f a)) -> Fix f b) -> (Fix f a -> b) -> Fix f b
pcoaugment g k = g (pextend (k . InB) . outB)
