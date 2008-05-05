-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Bifunctor
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD3
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (functional-dependencies)
--
-------------------------------------------------------------------------------------------
module Control.Bifunctor where

class Bifunctor f where
	bimap :: (a -> c) -> (b -> d) -> f a b -> f c d
	first :: (a -> c) -> f a b -> f c b
	first f = bimap f id
	second :: (b -> d) -> f a b -> f a d
	second = bimap id
