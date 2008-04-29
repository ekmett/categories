{-# OPTIONS -fglasgow-exts #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Pointed.Composition
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (functional-dependencies)
--
-------------------------------------------------------------------------------------------

module Control.Functor.Pointed.Composition where

import Control.Functor.Pointed
import Control.Functor.Composition.Class
import Control.Functor.Composition
import Control.Functor.Exponential
import Control.Functor.Full

newtype PointedCompF f g a = PointedCompF (CompF f g a) deriving (Functor, ExpFunctor, Full, Composition)

instance (Pointed f, Pointed g) => Pointed (PointedCompF f g) where
        point = compose . point . point

instance (Copointed f, Copointed g) => Copointed (PointedCompF f g) where
        copoint = copoint . copoint . decompose

newtype PostCompF mw f a = PostCompF (PointedCompF mw f a) deriving (Functor, ExpFunctor, Full, Composition, Pointed, Copointed)
newtype PreCompF f mw a  = PreCompF (PointedCompF f mw a) deriving (Functor, ExpFunctor, Full, Composition, Pointed, Copointed)
newtype DistCompF f g a  = DistCompF (PointedCompF f g a) deriving (Functor, ExpFunctor, Full, Composition, Pointed, Copointed)

