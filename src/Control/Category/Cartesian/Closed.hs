{-# LANGUAGE CPP #-}
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
module Control.Category.Cartesian.Closed
    (
    -- * Cartesian Closed Category
      CCC(..)
    , unitCCC, counitCCC
    -- * Co-(Cartesian Closed Category)
    , CoCCC(..)
    , unitCoCCC, counitCoCCC
    ) where

import Prelude ()
import qualified Prelude

import Control.Category
import Control.Category.Braided
import Control.Category.Cartesian

-- * Closed Cartesian Category

-- | A 'CCC' has full-fledged monoidal finite products and exponentials

-- Ideally you also want an instance for @'Bifunctor' ('Exp' hom) ('Dual' hom) hom hom@.
-- or at least @'Functor' ('Exp' hom a) hom hom@, which cannot be expressed in the constraints here.

class Cartesian k => CCC k where
    type Exp k :: * -> * -> *
    apply :: Product k (Exp k a b) a `k` b
    curry :: Product k a b `k` c -> a `k` Exp k b c
    uncurry :: a `k` Exp k b c -> Product k a b `k` c

instance CCC (->) where
  type Exp (->) = (->)
  apply (f,a) = f a
  curry = Prelude.curry
  uncurry = Prelude.uncurry

{-# RULES
"curry apply"         curry apply = id
-- "curry . uncurry"     curry . uncurry = id
-- "uncurry . curry"     uncurry . curry = id
 #-}

-- * Free @'Adjunction' (Product (<=) a) (Exp (<=) a) (<=) (<=)@
unitCCC :: CCC k => a `k` Exp k b (Product k b a)
unitCCC = curry braid

counitCCC :: CCC k => Product k b (Exp k b a) `k` a
counitCCC = apply . braid

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
