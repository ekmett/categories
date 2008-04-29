{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Bifunctor
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (functional-dependencies)
--
-- transform a pair of functors with a bifunctor deriving a new functor.
-- this subsumes functor product and functor coproduct
-------------------------------------------------------------------------------------------

module Control.Functor.Bifunctor where

import Control.Bifunctor
import Control.Functor.Contravariant
import Control.Functor.Exponential
import Control.Functor.Full
import Control.Functor.Pointed

-- * Bifunctor functor transformer

newtype BifunctorF p f g a = BifunctorF { runBifunctorF :: p (f a) (g a) }

instance (Bifunctor p, Functor f ,Functor g) => Functor (BifunctorF p f g) where
        fmap f = BifunctorF . bimap (fmap f) (fmap f) . runBifunctorF

instance (Bifunctor p, ContravariantFunctor f ,ContravariantFunctor g) => ContravariantFunctor (BifunctorF p f g) where
        contrafmap f = BifunctorF . bimap (contrafmap f) (contrafmap f) . runBifunctorF

instance (Bifunctor p, ExpFunctor f ,ExpFunctor g) => ExpFunctor (BifunctorF p f g) where
        xmap f g = BifunctorF . bimap (xmap f g) (xmap f g) . runBifunctorF



#ifndef __HADDOCK__
type (f :*: g) a = BifunctorF (,) f g a
#endif


runProductF :: BifunctorF (,) f g a -> (f a, g a)
runProductF = runBifunctorF

instance (Pointed f, Pointed g) => Pointed (BifunctorF (,) f g) where
        point = BifunctorF . (point &&& point)

instance (Faithful f, Faithful g) => Faithful (BifunctorF (,) f g)

#ifndef __HADDOCK__
type (f :+: g) a = BifunctorF Either f g a
#endif


runCoproductF :: BifunctorF Either f g a -> Either (f a) (g a)
runCoproductF = runBifunctorF

instance (Copointed f, Copointed g) => Copointed (BifunctorF Either f g) where
        copoint = (copoint ||| copoint) . runBifunctorF


instance (Pointed f, Pointed g) => Pointed (BifunctorF (,) f g) where
        point = BifunctorF . (point &&& point)

instance (Copointed f, Copointed g) => Copointed (BifunctorF Either f g) where
        copoint = (copoint ||| copoint) . runBifunctorF
