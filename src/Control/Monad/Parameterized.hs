{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Paramterized
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Monad.Parameterized 
	( Bifunctor(..)
	, PPointed(..)
	, PApplicative(..)
	, PMonad(..)
	, (>>*=), (=*<<), (>>*)
	, papPMonad
	) where

import Control.Bifunctor
import Control.Applicative.Parameterized

infixl 1 >>*=, >>*
infixr 1 =*<< 

class PApplicative f => PMonad f where
	pbind :: (a -> f b c) -> f a c -> f b c
	pbind f = pjoin . bimap f id
	pjoin :: f (f a b) b -> f a b
	pjoin = pbind id

papPMonad :: PMonad f => f (a -> b) c -> f a c -> f b c
papPMonad f x = f >>*= \ f' -> x >>*= \x' -> preturn (f' x')

(>>*=) :: PMonad f => f a c -> (a -> f b c) -> f b c
(>>*=) = flip pbind

(=*<<) :: PMonad f => (a -> f b c) -> f a c -> f b c
(=*<<) = pbind

(>>*) :: PMonad f => f a c -> f b c -> f b c 
m >>* n = m >>*= const n

{- Parameterized monad laws (from <http://crab.rutgers.edu/~pjohann/f14-ghani.pdf>)
> pbind preturn = id
> pbind g . preturn = g
> pbind (pbind g . j) = pbind g . pbind j
> pmap g . preturn = preturn
> pbind (pmap g . j) . pmap g = pmap g . pbind j 
-}

