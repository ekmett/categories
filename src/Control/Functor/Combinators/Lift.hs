{-# OPTIONS_GHC -cpp -fglasgow-exts -fallow-undecidable-instances #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Combinators.Lift
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

module Control.Functor.Combinators.Lift 
	( Lift(Lift,runLift)
	, (:*:), runProductF
	, (:+:), runCoproductF 
	, Ap, runAp, mkAp
	) where

import Control.Applicative
import Control.Category.Hask
import Control.Functor
import Control.Functor.Contra
import Control.Functor.Exponential
import Control.Functor.Full
import Control.Functor.HigherOrder
import Control.Monad.Identity
import Control.Functor.Pointed
import Control.Arrow ((&&&),(|||))

-- * Bifunctor functor transformer

-- type-level LiftA2 
newtype Lift p f g a = Lift { runLift :: p (f a) (g a) }
type Ap p = Lift p Identity

runAp :: Bifunctor p Hask Hask Hask => Ap p f a -> p a (f a)
runAp = first runIdentity . runLift

mkAp :: Bifunctor p Hask Hask Hask => p a (f a) -> Ap p f a 
mkAp = Lift . first Identity

instance (Bifunctor p Hask Hask Hask, Functor f ,Functor g) => Functor (Lift p f g) where
        fmap f = Lift . bimap (fmap f) (fmap f) . runLift

instance (Bifunctor p Hask Hask Hask, ContraFunctor f ,ContraFunctor g) => ContraFunctor (Lift p f g) where
        contramap f = Lift . bimap (contramap f) (contramap f) . runLift

instance (Bifunctor p Hask Hask Hask, ExpFunctor f ,ExpFunctor g) => ExpFunctor (Lift p f g) where
        xmap f g = Lift . bimap (xmap f g) (xmap f g) . runLift

instance (Bifunctor p Hask Hask Hask) => HFunctor (Ap p) where
        ffmap f = Lift . bimap (fmap f) (fmap f) . runLift
        hfmap f = Lift . second f . runLift


type (f :*: g) = Lift (,) f g

runProductF :: (f :*: g) a -> (f a, g a)
runProductF = runLift

instance (Pointed f, Pointed g) => Pointed (f :*: g) where
        point = Lift . (point &&& point)

instance (Applicative f, Applicative g) => Applicative (f :*: g) where
	pure b = Lift (pure b, pure b)
	Lift (f,g) <*> Lift (a,b) = Lift (f <*> a, g <*> b)

instance (Faithful f, Faithful g) => Faithful (f :*: g)

type (f :+: g) = Lift Either f g

runCoproductF :: (f :+: g) a -> Either (f a) (g a)
runCoproductF = runLift

instance (Copointed f, Copointed g) => Copointed (f :+: g) where
        extract = (extract ||| extract) . runLift
