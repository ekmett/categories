{-# LANGUAGE GADTs #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Discrete
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-------------------------------------------------------------------------------------------
module Control.Category.Discrete
	( Discrete(Refl)
	) where

import Prelude hiding (id,(.))
import Control.Category

data Discrete a b where 
	Refl :: Discrete a a

instance Category Discrete where
	id = Refl
	Refl . Refl = Refl

-- HasTerminalObject _|_ ?
