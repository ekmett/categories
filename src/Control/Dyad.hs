{-# OPTIONS_GHC -cpp #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Dyad
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD3
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-- Working Towards Maarten Fokkinga's Dyads
-------------------------------------------------------------------------------------------

module Control.Dyad where

import Prelude hiding (id,(.))
import Control.Category
import Control.Functor.Categorical

class (CDistributes w m (~>), CDistributes m w (~>), CExtend w (~>), CBind m (~>)) => CDyad w m (~>) where
	cdyid :: w a ~> m a

newtype DiKleisli w m (~>) a b = DiKleisli { runDiKleisli :: w a ~> m b }

-- instance CMonad m k => CFunctor (DiKleisli w m k a) k k where
--	cmap f (DiKleisli x) = DiKleisli (cmap f . x)

-- instance CMonad m k => QFunctor (DiKleisli w m k) k k where second g = 
-- instance CComonad w k => PFunctor (DiKleisli w m k) (Dual k) k where first f = 
-- instance (CMonad m k, CComonad w k) => Bifunctor (DiKleisli w m k) (Dual Hask) Hask Hask where bimap f g 

instance CDyad w m k => Category (DiKleisli w m k) where
	DiKleisli f . DiKleisli g = DiKleisli (cbind f . cdist . cextend g)
	id = DiKleisli cdyid
