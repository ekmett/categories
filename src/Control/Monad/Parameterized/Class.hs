{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Paramterized.Class
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- The notation >>*= was selected to indicate the kind of the parameter
-- in this case a simple type as opposed to >>*->*= for higher order monads.
----------------------------------------------------------------------------
module Control.Monad.Parameterized.Class where

import Control.Arrow ((|||), (+++))
-- import Control.Functor.Exponential
-- import Control.Functor.Contravariant
import Control.Monad
import Control.Bifunctor
import Control.Bifunctor.Fix

{- | Minimum definition:

1. preturn & pbind
2. preturn & pjoin

-}
	
class Bifunctor f => PMonad f where
	preturn :: a -> f a c
	pbind :: (a -> f b c) -> f a c -> f b c
	pbind f = pjoin . bimap f id
	pjoin :: f (f a b) b -> f a b
	pjoin = pbind id

(>>*=) :: PMonad f => f a c -> (a -> f b c) -> f b c
(>>*=) = flip pbind

(=*<<) :: PMonad f => (a -> f b c) -> f a c -> f b c
(=*<<) = pbind

-- (>>*) :: PMonad f => f a c -> f b c -> f b c 
m >>* n = m >>*= const n

-- bimapPMonad :: (PMonad f, Functor (f a)) => (a -> c) -> (b -> d) -> f a b -> f c d 
-- bimapPMonad f g xs = second g xs >>*= preturn . f

{- Parameterized monad laws (from <http://crab.rutgers.edu/~pjohann/f14-ghani.pdf>)
> pbind preturn = id
> pbind g . preturn = g
> pbind (pbind g . j) = pbind g . pbind j
> pmap g . preturn = preturn
> pbind (pmap g . j) . pmap g = pmap g . pbind j 
-}

