-------------------------------------------------------------------------------------------
-- |
-- Module	 : Control.Category.Dual
-- Copyright : 2008-2010 Edward Kmett
-- License	 : BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-------------------------------------------------------------------------------------------
module Control.Category.Dual
	( Dual(..)
	) where

import Prelude hiding ((.), id)
import Control.Category
import Data.Data

data Dual k a b = Dual { runDual :: k b a } 
    deriving (Data,Typeable)

instance Category k => Category (Dual k) where
	id = Dual id
	Dual f . Dual g = Dual (g . f)
