{-# OPTIONS -fglasgow-exts #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Cartesian
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-------------------------------------------------------------------------------------------
module Control.Category.Cartesian
	( module Control.Category.Associative
	, module Control.Category.Monoidal
	-- * Pre-(Co)Cartesian categories
	, PreCartesian(..)
	, bimapPreCartesian, braidPreCartesian, associatePreCartesian, coassociatePreCartesian
	, PreCoCartesian(..)
	, bimapPreCoCartesian, braidPreCoCartesian, associatePreCoCartesian, coassociatePreCoCartesian
	-- * (Co)Cartesian categories
	, Cartesian
	, CoCartesian
	) where

import Prelude hiding (Functor, map, (.), id, fst, snd, curry, uncurry)
import qualified Prelude

import Control.Functor
import Control.Category
import Control.Category.Associative
import Control.Category.Monoidal

infixr 3 &&&
infixr 2 |||

{- |
NB: This is weaker than traditional category with products! That is Cartesian, below.
The problem is @(->)@ lacks an initial object, since every type is inhabited in Haskell.
Consequently its coproduct is merely a semigroup, not a monoid as it has no identity, and 
since we want to be able to describe its dual category, which has this non-traditional 
form being built over a category with an associative bifunctor rather than as a monoidal category
for the product monoid.

Minimum definition: 

> fst, snd, diag 
> fst, snd, (&&&)
-}
class (Associative (Prod k) k, Coassociative (Prod k) k, Braided (Prod k) k) => PreCartesian k where
	type Prod k :: * -> * -> *
	fst :: k (Prod k a b) a
	snd :: k (Prod k a b) b
	diag :: k a (Prod k a a)
	(&&&) :: k a b -> k a c -> k a (Prod k b c)

	diag = id &&& id
	f &&& g = bimap f g . diag


{-# RULES
"fst . diag"  	fst . diag = id
"snd . diag"	snd . diag = id
"fst . f &&& g" forall f g. fst . (f &&& g) = f
"snd . f &&& g" forall f g. snd . (f &&& g) = g
 #-}

instance PreCartesian (->) where
	type Prod (->) = (,)
	fst = Prelude.fst
	snd = Prelude.snd
	diag a = (a,a)
	(f &&& g) a = (f a, g a)

-- alias
class (Monoidal (Prod k) k, PreCartesian k) => Cartesian k
instance (Monoidal (Prod k) k, PreCartesian k) => Cartesian k

-- | free construction of 'Bifunctor' for the product 'Bifunctor' @Prod k@ if @(&&&)@ is known
bimapPreCartesian :: PreCartesian k => k a c -> k b d -> k (Prod k a b) (Prod k c d)
bimapPreCartesian f g = (f . fst) &&& (g . snd)
	
-- | free construction of 'Braided' for the product 'Bifunctor' @Prod k@
braidPreCartesian :: PreCartesian k => k (Prod k a b) (Prod k b a)
braidPreCartesian = snd &&& fst

-- | free construction of 'Associative' for the product 'Bifunctor' @Prod k@
associatePreCartesian :: PreCartesian k => k (Prod k (Prod k a b) c) (Prod k a (Prod k b c))
associatePreCartesian = (fst . fst) &&& first snd

-- | free construction of 'Coassociative' for the product 'Bifunctor' @Prod k@
coassociatePreCartesian :: PreCartesian k => k (Prod k a (Prod k b c)) (Prod k (Prod k a b) c)
coassociatePreCartesian = braid . second braid . associatePreCartesian . first braid . braid 

-- * Co-PreCartesian categories

-- a category that has finite coproducts, weakened the same way as PreCartesian above was weakened
class (Associative (Sum k) k, Coassociative (Sum k) k, Braided (Sum k) k) => PreCoCartesian k where
#ifndef __HADDOCK__
	type Sum k :: * -> * -> *
#endif
	inl :: k a (Sum k a b)
	inr :: k b (Sum k a b)
	codiag :: k (Sum k a a) a
	(|||) :: k a c -> k b c -> k (Sum k a b) c

	codiag = id ||| id
	f ||| g = codiag . bimap f g

{-# RULES
"codiag . inl"  codiag . inl = id
"codiag . inr"	codiag . inr = id
"(f ||| g) . inl" forall f g. (f ||| g) . inl = f
"(f ||| g) . inr" forall f g. (f ||| g) . inr = g
 #-}

instance PreCoCartesian (->) where
	type Sum (->) = Either
	inl = Left
	inr = Right
	codiag (Left a) = a
	codiag (Right a) = a
	(f ||| _) (Left a) = f a 
	(_ ||| g) (Right a) = g a

-- | free construction of 'Bifunctor' for the coproduct 'Bifunctor' @Sum k@ if @(|||)@ is known
bimapPreCoCartesian :: PreCoCartesian k => k a c -> k b d -> k (Sum k a b) (Sum k c d)
bimapPreCoCartesian f g = (inl . f) ||| (inr . g)

-- | free construction of 'Braided' for the coproduct 'Bifunctor' @Sum k@
braidPreCoCartesian :: PreCoCartesian k => k (Sum k a b) (Sum k b a)
braidPreCoCartesian = inr ||| inl

-- | free construction of 'Associative' for the coproduct 'Bifunctor' @Sum k@
associatePreCoCartesian :: PreCoCartesian k => k (Sum k (Sum k a b) c) (Sum k a (Sum k b c))
associatePreCoCartesian = braid . first braid . coassociatePreCoCartesian . second braid . braid

-- | free construction of 'Coassociative' for the coproduct 'Bifunctor' @Sum k@
coassociatePreCoCartesian :: PreCoCartesian k => k (Sum k a (Sum k b c)) (Sum k (Sum k a b) c)
coassociatePreCoCartesian = (inl . inl) ||| first inr

class (Comonoidal (Sum k) k, PreCoCartesian k) => CoCartesian k
instance (Comonoidal (Sum k) k, PreCoCartesian k) => CoCartesian k
