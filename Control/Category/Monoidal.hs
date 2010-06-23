{-# OPTIONS_GHC -fglasgow-exts #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Monoidal
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-- A 'Monoidal' category is a category with an associated biendofunctor that has an identity,
-- which satisfies Mac Lane''s pentagonal and triangular coherence conditions
-- Technically we usually say that category is 'monoidal', but since
-- most interesting categories in our world have multiple candidate bifunctors that you can 
-- use to enrich their structure, we choose here to think of the bifunctor as being 
-- monoidal. This lets us reuse the same Bifunctor over different categories without 
-- painful type annotations.

-- The use of class associated types here makes Control.Category.Cartesian FAR more palatable
-------------------------------------------------------------------------------------------

module Control.Category.Monoidal 
	( module Control.Category.Braided
	, HasIdentity
	, Monoidal(..)
	, Comonoidal(..)
	) where

import Control.Category.Braided
import Control.Category.Hask
import Control.Category.Associative
import Control.Functor
import Data.Void

-- | Denotes that we have some reasonable notion of 'Identity' for a particular 'Bifunctor' in this 'Category'. This
-- notion is currently used by both 'Monoidal' and 'Comonoidal'
class Bifunctor p k k k => HasIdentity k p i | k p -> i 

{- | A monoidal category. 'idl' and 'idr' are traditionally denoted lambda and rho
 the triangle identity holds:

> bimap idr id = bimap id idl . associate 
> bimap id idl = bimap idr id . associate
-}

class (Associative k p, HasIdentity k p i) => Monoidal k p i | k p -> i where
	idl :: k (p i a) a
	idr :: k (p a i) a

{- | A comonoidal category satisfies the dual form of the triangle identities

> bimap idr id = coassociate . bimap id idl
> bimap id idl = coassociate . bimap idr id

This type class is also (ab)used for the inverse operations needed for a strict (co)monoidal category.
A strict (co)monoidal category is one that is both 'Monoidal' and 'Comonoidal' and satisfies the following laws:

> idr . coidr = id 
> idl . coidl = id 
> coidl . idl = id 
> coidr . idr = id 

-}
class (Coassociative k p, HasIdentity k p i) => Comonoidal k p i | k p -> i where
	coidl :: k a (p i a)
	coidr :: k a (p a i)

{-# RULES
-- "bimap id idl/associate" 		bimap id idl . associate = bimap idr id
-- "bimap idr id/associate" 		bimap idr id . associate = bimap id idl
-- "coassociate/bimap id idl"  		coassociate . bimap id idl = bimap idr id
-- "coassociate/bimap idr id"  		coassociate . bimap idr id = bimap id idl
"idr/coidr" 			idr . coidr = id
"idl/coidl"			idl . coidl = id
"coidl/idl"			coidl . idl = id
"coidr/idr"			coidr . idr = id
"idr/braid"                     idr . braid = idl
"idl/braid"                     idl . braid = idr
"braid/coidr"                   braid . coidr = coidl
"braid/coidl"                   braid . coidl = coidr
 #-}

instance HasIdentity Hask (,) Void


instance Monoidal Hask (,) Void where
        idl = snd
        idr = fst

