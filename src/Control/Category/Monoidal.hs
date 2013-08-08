{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-------------------------------------------------------------------------------------------
-- |
-- Copyright : 2008-2013 Edward Kmett
-- License   : BSD
--
-- Maintainer : Edward Kmett <ekmett@gmail.com>
-- Stability  : experimental
-- Portability: non-portable (class-associated types)
--
-- A 'Monoidal' category is a category with an associated biendofunctor that has an identity,
-- which satisfies Mac Lane''s pentagonal and triangular coherence conditions
-- Technically we usually say that category is 'Monoidal', but since
-- most interesting categories in our world have multiple candidate bifunctors that you can
-- use to enrich their structure, we choose here to think of the bifunctor as being
-- monoidal. This lets us reuse the same 'Bifunctor' over different categories without
-- painful newtype wrapping.
--
-------------------------------------------------------------------------------------------
module Control.Category.Monoidal
  ( MonoidalBifunctor(..)
  , Monoidal
  , Product
  , Sum
  ) where

import Control.Categorical.Bifunctor
import Control.Categorical.Category
import Data.Void
import Prelude hiding (id,(.))

-- |
-- @'Monoidal' k p@ indicates that the category @k@ forms a monoidal category using @p@ as the choice of
-- 'Bifunctor'.
--
-- We choose to talk about the 'Bifunctor' being \"monoidal\" rather than the 'Category', as it is much easier
-- to work with the instances in this fashion than to work with newtypes on '(->)' all over the place in Haskell,
-- since we can usually use type inference to figure out which monoidal category is under consideration.

class Bifunctor p k k k => MonoidalBifunctor (k :: x -> x -> *) (p :: x -> x -> x) where
  -- | The identity object.
  type Id k p :: x

  -- | The associator, traditionally denoted @α@, is a natural isomorphism.
  --
  -- @
  -- associate . disassociate = id
  -- disassociate . associate = id
  -- @
  --
  -- that satisfies Mac Lane\'s pentagonal coherence condition:
  --
  -- @
  -- second associate . associate . first associate = associate . associate
  -- @
  associate :: k (p (p a b) c) (p a (p b c))
  disassociate :: k (p a (p b c)) (p (p a b) c)

  -- | The 'left unitor', traditionally denoted \"λ\", is a natural isomorphism
  --
  -- @
  -- idr . unidr = id
  -- unidr . idr = id
  -- @
  --
  -- The unitors are expected to satisfy the triangle identities:
  --
  -- @
  -- first idr = second idl . associate
  -- second idl = first idr . associate
  -- first idr = disassociate . second idl
  -- second idl = disassociate . first idr
  -- @
  --
  idl   :: k (p (Id k p) a) a
  unidl :: k a (p (Id k p) a)

  -- | The 'right unitor', traditionally denoted \"ρ\", is a natural isomorphism
  --
  -- @
  -- idl . unidl = id
  -- unidl . idl = id
  -- @
  idr   :: k (p a (Id k p)) a
  unidr :: k a (p a (Id k p))

-- | Sometimes it does matter what bifunctor is associated with a category "canonically",
-- so we can single out a 'Product'.
type family Product (k :: x -> x -> *) :: x -> x -> x
type instance Product (->) = (,)

type family Sum (k :: x -> x -> *) :: x -> x -> x
type instance Sum (->) = Either

class MonoidalBifunctor k (Product k) => Monoidal k
instance MonoidalBifunctor k (Product k) => Monoidal k

instance MonoidalBifunctor (->) (,) where
  type Id (->) (,) = ()

  associate ((a,b),c) = (a,(b,c))
  {-# INLINE associate #-}

  disassociate (a,(b,c)) = ((a,b),c)
  {-# INLINE disassociate #-}

  idl = snd
  {-# INLINE idl #-}
  unidl a = ((),a)
  {-# INLINE unidl #-}

  idr = fst
  {-# INLINE idr #-}

  unidr a = (a,())
  {-# INLINE unidr #-}

instance MonoidalBifunctor (->) Either where
  type Id (->) Either = Void

  associate (Left (Left a)) = Left a
  associate (Left (Right b)) = Right (Left b)
  associate (Right c) = Right (Right c)
  {-# INLINE associate #-}

  disassociate (Left a) = Left (Left a)
  disassociate (Right (Left b)) = Left (Right b)
  disassociate (Right (Right c)) = Right c
  {-# INLINE disassociate #-}

  idl = either absurd id
  {-# INLINE idl #-}
  unidl = Right
  {-# INLINE unidl #-}

  idr = either id absurd
  {-# INLINE idr #-}
  unidr = Left
  {-# INLINE unidr #-}
