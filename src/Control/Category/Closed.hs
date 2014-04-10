{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, TypeOperators, FlexibleContexts #-}
-------------------------------------------------------------------------------------------
-- |
-- Module     : Control.Category.Cartesian.Closed
-- Copyright  : 2008 Edward Kmett
-- License    : BSD
--
-- Maintainer : Edward Kmett <ekmett@gmail.com>
-- Stability  : experimental
-- Portability: non-portable (class-associated types)
--
-------------------------------------------------------------------------------------------
module Control.Category.Closed
    (
    -- * Cartesian Closed Category
      Closed(..)
    , unitClosed, counitClosed
    , ClosedBraided
{-
    , unitCCC, counitCCC
    -- * Co-(Cartesian Closed Category)
    , CoCCC(..)
    , unitCoCCC, counitCoCCC
-}
    ) where

import Prelude ()
import qualified Prelude

import Control.Category
import Control.Category.Braided
import Control.Category.Monoidal
-- import Control.Category.Cartesian

-- | A /left/-'Closed' monoidal category has full-fledged monoidal finite products and exponentials.
--
-- This is /left/-'Closed' following the conventions for @Set@ and @Hask@, but not the conventions
-- for @Tang@, @Hilb@, @n-Cob@ or most other monoidal categories.
--
-- Ideally you also want an instance for @'Bifunctor' ('Exp' hom) ('Dual' hom) hom hom@.
-- or at least @'Functor' ('Exp' hom a) hom hom@, which cannot be expressed in the constraints here.
--
-- A /left/-closed 'MonoidalCategory'
class Monoidal k => Closed (k :: x -> x -> *) where
  type Exp k :: x -> x -> x
  apply :: Product k (Exp k a b) a `k` b
  curry :: Product k a b `k` c -> a `k` Exp k b c
  uncurry :: a `k` Exp k b c -> Product k a b `k` c

instance Closed (->) where
  type Exp (->) = (->)
  apply (f,a) = f a
  curry = Prelude.curry
  uncurry = Prelude.uncurry

-- * Free @'Adjunction' (Product (<=) a) (Exp (<=) a) (<=) (<=)@
unitClosed :: (Closed k, Braided k) => a `k` Exp k b (Product k b a)
unitClosed = curry braid

counitClosed :: (Closed k, Braided k) => Product k b (Exp k b a) `k` a
counitClosed = apply . braid

-- | A 'Closed' 'Braided' 'Monoidal' 'Category'
class (Closed k, Braided k) => ClosedBraided k
instance (Closed k, Braided k) => ClosedBraided k

{-

-- * A Co-(Closed Cartesian Category)

-- | A Co-CCC has full-fledged comonoidal finite coproducts and coexponentials

-- You probably also want an instance for @'Bifunctor' ('coexp' hom) ('Dual' hom) hom hom@.

class CoCartesian k => CoCCC k where
    type Coexp k :: * -> * -> *
    coapply :: b `k` Sum k (Coexp k a b) a
    cocurry :: c `k` Sum k a b -> Coexp k b c `k` a
    uncocurry :: Coexp k b c `k` a -> c `k` Sum k a b

{-# RULES
"cocurry coapply" cocurry coapply = id
-- "cocurry . uncocurry"   cocurry . uncocurry = id
-- "uncocurry . cocurry"   uncocurry . cocurry = id
 #-}

-- * Free @'Adjunction' ('Coexp' (<=) a) ('Sum' (<=) a) (<=) (<=)@
unitCoCCC :: CoCCC k => a `k` Sum k b (Coexp k b a)
unitCoCCC = swap . coapply

counitCoCCC :: CoCCC k => Coexp k b (Sum k b a) `k` a
counitCoCCC = cocurry swap
-}
