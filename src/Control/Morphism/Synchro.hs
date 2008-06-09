{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Morphism.Synchro
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
--
-- Martin Erwig's synchromorphisms.
----------------------------------------------------------------------------
module Control.Morphism.Synchro where

import Control.Category.Cartesian ((&&&))
import Control.Category.Hask
import Control.Functor
import Control.Functor.Algebra

-- | @synchro d' f d g1 g2 d''@ is Martin Erwig's @d,d''-synchromorphism to d'@. Mostly useful for graph algorithms.
synchro :: QFunctor h Hask Hask => Bialgebra m n c -> (h x (Either a c) -> m c) -> Trialgebra (f x) (g x) (h x) a -> ((h x a, b) -> k x b) -> ((h x a, j x b) -> h x (Either a (g x a, b))) -> Bialgebra (k x) (j x) b -> (g x a, b) -> c 

--             g1
-- h = D' <- D <-> D''
--       f     g2
-- dfs = List <- Graph <-> Stack -- depth-first search
-- bfs = List <- Graph <-> Queue -- breadth-first search

synchro d' f d g1 g2 d'' = h where
	h = fst d' . f . second (second h) . g2 . (fst &&& (snd d'' . fst d'' . g1)) . first (snd d)
	-- (g x a, b) 			>- first (snd d)  ->
	-- (h x a, b) 			>- (fst &&& g1) ->
	-- (h x a, k x b) 		>- second (fst d'') ->
	-- (h x a, b) 			>- second (snd d'') ->
	-- (h x a, j x b)		>- g2 ->
	-- (h x (Either a (g x a, b)) 	>- second (second h) ->
	-- (h x (Either a c))		>- f ->
	-- m c				>- fst d'
	-- c
