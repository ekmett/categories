-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Bifunctor.Monoidal
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
-------------------------------------------------------------------------------------------
module Control.Bifunctor.Monoidal where

import Control.Bifunctor
import Control.Bifunctor.Associative
import Control.Bifunctor.Braided

-- | Denotes that we have some reasonable notion of 'Identity' for a particular 'Bifunctor' in this 'Category'. This
-- notion is currently used by both 'Monoidal' and 'Comonoidal'
class Bifunctor p => HasIdentity p i | p -> i 

{- | A monoidal category. 'idl' and 'idr' are traditionally denoted lambda and rho
 the triangle identity holds:

> bimap idr id = bimap id idl . associate 
> bimap id idl = bimap idr id . associate
-}

class (Associative p, HasIdentity p i) => Monoidal p i | p -> i where
	idl :: p i a -> a
	idr :: p a i -> a

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
class (Coassociative p, HasIdentity p i) => Comonoidal p i | p -> i where
	coidl :: a -> p i a
	coidr :: a -> p a i

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

