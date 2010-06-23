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

import Control.Category.Hask
import Control.Category.Associative
import Control.Category.Monoidal
import Prelude hiding (Functor, map, (.), id, fst, snd, curry, uncurry)
import qualified Prelude (fst,snd)
import Control.Functor
import Control.Category

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
class (Associative k p, Coassociative k p, Braided k p) => PreCartesian k p | k -> p where
	fst :: k (p a b) a
	snd :: k (p a b) b
	diag :: k a (p a a)
	(&&&) :: k a b -> k a c -> k a (p b c)

	diag = id &&& id
	f &&& g = bimap f g . diag


{-# RULES
"fst . diag"  	fst . diag = id
"snd . diag"	snd . diag = id
"fst . f &&& g" forall f g. fst . (f &&& g) = f
"snd . f &&& g" forall f g. snd . (f &&& g) = g
 #-}

instance PreCartesian Hask (,) where
	fst = Prelude.fst
	snd = Prelude.snd
	diag a = (a,a)
	(f &&& g) a = (f a, g a)

-- alias
class (Monoidal k p i, PreCartesian k p) => Cartesian k p i | k -> p i 
instance (Monoidal k p i, PreCartesian k p) => Cartesian k p i

-- | free construction of 'Bifunctor' for the product 'Bifunctor' @Prod k@ if @(&&&)@ is known
bimapPreCartesian :: PreCartesian k p => k a c -> k b d -> k (p a b) (p c d)
bimapPreCartesian f g = (f . fst) &&& (g . snd)
	
-- | free construction of 'Braided' for the product 'Bifunctor' @Prod k@
braidPreCartesian :: PreCartesian k p => k (p a b) (p b a)
braidPreCartesian = snd &&& fst

-- | free construction of 'Associative' for the product 'Bifunctor' @Prod k@
associatePreCartesian :: PreCartesian k p => k (p (p a b) c) (p a (p b c))
associatePreCartesian = (fst . fst) &&& first snd

-- | free construction of 'Coassociative' for the product 'Bifunctor' @Prod k@
coassociatePreCartesian :: PreCartesian k p => k (p a (p b c)) (p (p a b) c)
coassociatePreCartesian = braid . second braid . associatePreCartesian . first braid . braid 

-- * Co-PreCartesian categories

-- a category that has finite coproducts, weakened the same way as PreCartesian above was weakened
class (Associative k s, Coassociative k s , Braided k s) => PreCoCartesian k s | k -> s where
	inl :: k a (s a b)
	inr :: k b (s a b)
	codiag :: k (s a a) a
	(|||) :: k a c -> k b c -> k (s a b) c

	codiag = id ||| id
	f ||| g = codiag . bimap f g

{-# RULES
"codiag . inl"  codiag . inl = id
"codiag . inr"	codiag . inr = id
"(f ||| g) . inl" forall f g. (f ||| g) . inl = f
"(f ||| g) . inr" forall f g. (f ||| g) . inr = g
 #-}

instance PreCoCartesian Hask Either where
	inl = Left
	inr = Right
	codiag (Left a) = a
	codiag (Right a) = a
	(f ||| _) (Left a) = f a 
	(_ ||| g) (Right a) = g a

-- | free construction of 'Bifunctor' for the coproduct 'Bifunctor' @Sum k@ if @(|||)@ is known
bimapPreCoCartesian :: PreCoCartesian k s => k a c -> k b d -> k (s a b) (s c d)
bimapPreCoCartesian f g = (inl . f) ||| (inr . g)

-- | free construction of 'Braided' for the coproduct 'Bifunctor' @Sum k@
braidPreCoCartesian :: PreCoCartesian k s => k (s a b) (s b a)
braidPreCoCartesian = inr ||| inl

-- | free construction of 'Associative' for the coproduct 'Bifunctor' @Sum k@
associatePreCoCartesian :: PreCoCartesian k s => k (s (s a b) c) (s a (s b c))
associatePreCoCartesian = braid . first braid . coassociatePreCoCartesian . second braid . braid

-- | free construction of 'Coassociative' for the coproduct 'Bifunctor' @Sum k@
coassociatePreCoCartesian :: PreCoCartesian k s => k (s a (s b c)) (s (s a b) c)
coassociatePreCoCartesian = (inl . inl) ||| first inr

class (Comonoidal k s i, PreCoCartesian k s) => CoCartesian k s i | k -> s i
instance (Comonoidal k s i, PreCoCartesian k s) => CoCartesian k s i 
