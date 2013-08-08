{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-------------------------------------------------------------------------------------------
-- |
-- Copyright  : 2008-2013 Edward Kmett
-- License    : BSD
--
-- Maintainer : Edward Kmett <ekmett@gmail.com>
-- Stability  : experimental
-- Portability: non-portable
--
-------------------------------------------------------------------------------------------
module Control.Category.Braided
  ( BraidedBifunctor(..)
  , SymmetricBifunctor
  , swap
  , Braided
  , Symmetric
  ) where

-- import Control.Categorical.Bifunctor
import Control.Category.Monoidal

-- |
-- A 'Braided' 'Monoidal' 'Category' can commute the arguments of its associated 'Bifunctor'.
--
class MonoidalBifunctor k p => BraidedBifunctor (k :: x -> x -> *) (p :: x -> x -> x) where
    -- | 'braid' is a natural isomorphism that represents the commutativity constraint on a braided monoidal category.
    --
    -- @
    -- 'braid' . 'unbraid' = 'id'
    -- 'unbraid' . 'braid' = 'id'
    -- @
    --
    -- It satisfies the hexagon axioms
    --
    -- @
    -- associate . braid . associate = second braid . associate . first braid
    -- disassociate . braid . disassociate = first braid . disassociate . second braid
    -- @
    --
    -- and braiding commutes with units
    --
    -- @
    -- idr . braid = idl
    -- idl . braid = idr
    -- @
    braid :: k (p a b) (p b a)

    -- | The default definition of 'unbraid' is given in terms of 'braid' for 'Symmetric' monoidal categories.
    unbraid :: k (p b a) (p a b)
    default unbraid :: SymmetricBifunctor k p => k (p b a) (p a b)
    unbraid = braid

-- | A 'Braided' 'Monoidal' 'Category'
class BraidedBifunctor k (Product k) => Braided (k :: x -> x -> *)
instance BraidedBifunctor k (Product k) => Braided k

instance BraidedBifunctor (->) Either where
    braid (Left a) = Right a
    braid (Right b) = Left b

instance BraidedBifunctor (->) (,) where
    braid ~(a,b) = (b,a)

-- |
-- If we have a 'Symmetric' 'Monoidal' 'Category', you get the additional law:
--
-- @
-- swap . swap = id
-- @
--
-- and it follows that
--
-- @
-- 'unbraid' = 'braid' = 'swap'
-- @
class BraidedBifunctor k p => SymmetricBifunctor k p

class SymmetricBifunctor k (Product k) => Symmetric k
instance SymmetricBifunctor k (Product k) => Symmetric k

swap :: SymmetricBifunctor k p => k (p a b) (p b a)
swap = braid

instance SymmetricBifunctor (->) Either
instance SymmetricBifunctor (->) (,)
