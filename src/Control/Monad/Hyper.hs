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

module Control.Monad.Hyper 
	( ContraFunctor(..)
	, Hyper
	, Hyp
	, HyperB(..)
	) where

import Control.Category.Hask
import Control.Functor
import Control.Functor.Fix
import Control.Functor.Contra
import Control.Monad.Instances
import Control.Monad.Parameterized

newtype HyperB h a b = HyperB { runHyperB :: h b -> a } 

instance PFunctor (HyperB h) Hask Hask where
	first f h = HyperB (f . runHyperB h)

instance ContraFunctor h => QFunctor (HyperB h) Hask Hask where
	second g h = HyperB (runHyperB h . contramap g)

instance ContraFunctor h => Bifunctor (HyperB h) Hask Hask Hask where
	bimap f g h = HyperB (f . runHyperB h . contramap g)

instance ContraFunctor h => PPointed (HyperB h) where
	preturn = HyperB . const

instance ContraFunctor h => PApplicative (HyperB h) where
	pap = papPMonad

instance ContraFunctor h => PMonad (HyperB h) where
	pbind k (HyperB h) = HyperB (k . h >>= runHyperB)

-- | A generic recursive hyperfunction-like combinator
type Hyper h a = FixB (HyperB h)

-- | Traditional Hyper functions
type Hyp e a = Hyper (ContraF e) a

