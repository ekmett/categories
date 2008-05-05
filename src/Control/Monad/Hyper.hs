-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Monad.Hyper
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (functional-dependencies)
--
-- Based on the construction of hyperfunctions as parameterized monads in 
-- <http://crab.rutgers.edu/~pjohann/f14-ghani.pdf>
-------------------------------------------------------------------------------------------

module Control.Monad.Hyper where

import Control.Bifunctor
import Control.Bifunctor.Fix
import Control.Monad.Parameterized.Class
import Control.Functor.Contravariant
import Control.Monad.Instances

newtype HyperB h a b = HyperB { runHyperB :: h b -> a } 

instance ContravariantFunctor h => Bifunctor (HyperB h) where
	bimap f g h = HyperB (f . runHyperB h . contramap g)

instance ContravariantFunctor h => PMonad (HyperB h) where
	preturn = HyperB . const
	pbind k (HyperB h) = HyperB (k . h >>= runHyperB) -- the bind is in the (->)e monad

-- | A generic recursive hyperfunction-like combinator
type Hyper h a = FixB (HyperB h)

-- | Traditional Hyper functions
type Hyp e a = Hyper (ContraF e) a

