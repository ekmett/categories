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
import Control.Category.Hask
import Control.Category.Cartesian

-- | the canonical factoring morphism 
factor :: (PreCartesian hom prod, PreCoCartesian hom sum) => hom (sum (prod a b) (prod a c)) (prod a (sum b c))
factor = second inl ||| second inr

-- | A category in which 'factor' is an isomorphism
class (PreCartesian hom prod, PreCoCartesian hom sum) => Distributive hom prod sum where
	distribute :: hom (prod a (sum b c)) (sum (prod a b) (prod a c))

instance Distributive Hask (,) Either where
	distribute (a,Left b) = Left (a,b)
	distribute (a,Right c) = Right (a,c)

{-# RULES
"factor . distribute"	 factor . distribute = id
"distribute . factor"    distribute . factor = id
 #-}
