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
	, PHyper(..)
	) where

import Control.Category.Hask
import Control.Functor
import Control.Functor.Fix
import Control.Functor.Contra
import Control.Monad.Instances
import Control.Monad.Parameterized

newtype PHyper h a b = PHyper { runPHyper :: h b -> a } 

instance PFunctor (PHyper h) Hask Hask where
	first f h = PHyper (f . runPHyper h)

instance ContraFunctor h => QFunctor (PHyper h) Hask Hask where
	second g h = PHyper (runPHyper h . contramap g)

instance ContraFunctor h => Bifunctor (PHyper h) Hask Hask Hask where
	bimap f g h = PHyper (f . runPHyper h . contramap g)

instance ContraFunctor h => PPointed (PHyper h) where
	preturn = PHyper . const

instance ContraFunctor h => PApplicative (PHyper h) where
	pap = papPMonad

instance ContraFunctor h => PMonad (PHyper h) where
	pbind k (PHyper h) = PHyper (k . h >>= runPHyper)

-- | A generic recursive hyperfunction-like combinator
type Hyper h a = Fix (PHyper h)

-- | Traditional Hyper functions
type Hyp e a = Hyper (ContraF e) a
