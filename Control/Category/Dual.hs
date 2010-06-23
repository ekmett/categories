-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Dual
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: semi-portable (optional class-associated types)
--
-------------------------------------------------------------------------------------------
module Control.Category.Dual
	( Dual(..)
	) where

import Prelude hiding ((.), id)
import Control.Category

data Dual k a b = Dual { runDual :: k b a } 

instance Category k => Category (Dual k) where
	id = Dual id
	Dual f . Dual g = Dual (g . f)
