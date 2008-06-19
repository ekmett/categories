{-# LANGUAGE GADTs #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Allegory
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-- Allegories are generalizations of categories to cover relations.
-------------------------------------------------------------------------------------------
module Control.Allegory where

import Prelude hiding (id,(.),all)
import Control.Category
import Control.Functor.Categorical
infix 5 .<=.

{-
	An allegory is a category in which every arrow has a partial ordering, meet and converse such that:
	converse f . converse g = converse (f . g)
	f .<=. converse g = converse f .<=. g

	Allegories are to relations what categories are to functions
-}
class Category k => Allegory k where
	(.<=.) :: k a b -> k a b -> Bool
	meet :: k a b -> k a b -> k a b
	converse :: k a b -> k b a

	isSimple :: k a b -> Bool
	isSimple r = r . converse r .<=. id

	isTotal :: k a b -> Bool
	isTotal r = id .<=. converse r . r

	isMap :: k a b -> Bool
	isMap r = isSimple r && isTotal r

class Allegory k => TabulatedAllegory k f where
	tabulateLeft  :: k a b -> k a (f a b)
	tabulateRight :: k a b -> k b (f a b)

class Allegory k => UnitalAllegory k i | k -> i where
	-- unit of the allegory
	all :: k a i

	rightDomain :: k b a -> k b b
	rightDomain f = converse all . all . f

	leftDomain :: k b a -> k a a 
	leftDomain f = f . converse all . all

class (Allegory k1, Allegory k2, CFunctor f k1 k2) => Relator f k1 k2

data Map k a b = Map { runMap :: k a b } 

-- the sub-category of maps in an Allegory
instance Allegory k => Category (Map k) where
	id = Map id
	Map f . Map g = Map (f . g)

extractMap :: Allegory k => k a b -> Maybe (Map k a b)
extractMap f = if isMap f then Just (Map f) else Nothing
