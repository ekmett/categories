-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Distributive
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-------------------------------------------------------------------------------------------
module Control.Category.Distributive
	( 
	-- * Distributive Categories
	  factor
	, Distributive(..)
	) where

import Prelude hiding (Functor, map, (.), id, fst, snd, curry, uncurry)
import Control.Functor
import Control.Category
import Control.Category.Cartesian

-- | the canonical factoring morphism 
factor :: (Cartesian k, CoCartesian k) => k (Sum k (Prod k a b) (Prod k a c)) (Prod k a (Sum k b c))
factor = second inl ||| second inr

-- | A category in which 'factor' is an isomorphism
class (Cartesian k, CoCartesian k) => Distributive k where
	distribute :: k (Prod k a (Sum k b c)) (Sum k (Prod k a b) (Prod k a c))

{-# RULES
"factor . distribute"	 factor . distribute = id
"distribute . factor"    distribute . factor = id
 #-}
