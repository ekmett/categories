{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	 : Control.Category.Monoidal
-- Copyright : 2008 Edward Kmett
-- License	 : BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-- A 'Monoidal' category is a category with an associated biendofunctor that has an identity,
-- which satisfies Mac Lane''s pentagonal and triangular coherence conditions
-- Technically we usually say that category is 'Monoidal', but since
-- most interesting categories in our world have multiple candidate bifunctors that you can 
-- use to enrich their structure, we choose here to think of the bifunctor as being 
-- monoidal. This lets us reuse the same 'Bifunctor' over different categories without 
-- painful newtype wrapping.

-- The use of class associated types here makes Control.Category.Cartesian FAR more palatable
-------------------------------------------------------------------------------------------

module Control.Category.Monoidal 
	( HasIdentity(..)
	, Monoidal(..)
	, Comonoidal(..)
	) where

import Control.Category.Braided
import Control.Category.Associative
import Control.Categorical.Bifunctor
import Data.Void

-- | Denotes that we have some reasonable notion of 'Identity' for a particular 'Bifunctor' in this 'Category'. This
-- notion is currently used by both 'Monoidal' and 'Comonoidal'
class Bifunctor p k k k => HasIdentity k p where
    type Id k p :: *

{- | A monoidal category. 'idl' and 'idr' are traditionally denoted lambda and rho
 the triangle identity holds:

> first idr = second idl . associate 
> second idl = first idr . associate
-}

class (Associative k p, HasIdentity k p) => Monoidal k p where
	idl :: k (p (Id k p) a) a
	idr :: k (p a (Id k p)) a

{- | A comonoidal category satisfies the dual form of the triangle identities

> first idr = disassociate . second idl
> second idl = disassociate . first idr

This type class is also (ab)used for the inverse operations needed for a strict (co)monoidal category.
A strict (co)monoidal category is one that is both 'Monoidal' and 'Comonoidal' and satisfies the following laws:

> idr . coidr = id 
> idl . coidl = id 
> coidl . idl = id 
> coidr . idr = id 

-}
class (Disassociative k p, HasIdentity k p) => Comonoidal k p where
	coidl :: k a (p (Id k p) a)
	coidr :: k a (p a (Id k p))

{-# RULES
-- "bimap id idl/associate" 	second idl . associate = first idr
-- "bimap idr id/associate" 	first idr . associate = second idl
-- "disassociate/bimap id idl"  disassociate . second idl = first idr
-- "disassociate/bimap idr id"  disassociate . first idr = second idl
"idr/coidr" idr . coidr = id
"idl/coidl"	idl . coidl = id
"coidl/idl"	coidl . idl = id
"coidr/idr"	coidr . idr = id
"idr/braid" idr . braid = idl
"idl/braid" idl . braid = idr
"braid/coidr" braid . coidr = coidl
"braid/coidl" braid . coidl = coidr
 #-}

instance HasIdentity (->) (,) where
    type Id (->) (,) = Void

instance Monoidal (->) (,) where
        idl = snd
        idr = fst

