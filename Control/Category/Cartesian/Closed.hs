{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, TypeOperators, FlexibleContexts #-}
-------------------------------------------------------------------------------------------
-- |
-- Module     : Control.Category.Cartesian.Closed
-- Copyright : 2008 Edward Kmett
-- License     : BSD
--
-- Maintainer    : Edward Kmett <ekmett@gmail.com>
-- Stability    : experimental
-- Portability    : non-portable (class-associated types)
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

import Prelude () -- hiding ((.), id, fst, snd, curry, uncurry)

import Control.Category
import Control.Category.Braided
import Control.Category.Cartesian
import Control.Category.Monoidal

-- * Closed Cartesian Category 

-- | A 'CCC' has full-fledged monoidal finite products and exponentials

-- Ideally you also want an instance for @'Bifunctor' ('Exp' hom) ('Dual' hom) hom hom@.
-- or at least @'Functor' ('Exp' hom a) hom hom@, which cannot be expressed in the constraints here.

class ( Cartesian (<=)
      , Symmetric (<=) (Product (<=))
      , Monoidal (<=) (Product (<=)) 
      ) => CCC (<=) where
    type Exp (<=) :: * -> * -> *
    -- apply :: (<\>) ~ Exp (<=), (<*>) ~ Product (<=) => ((a <\> b) <*> a) <= b
    apply :: (Product (<=) (Exp (<=) a b) a) <= b
    curry :: ((Product (<=) a b) <= c) -> a <= Exp (<=) b c
    uncurry :: (a <= (Exp (<=) b c)) -> (Product (<=>) a b <= c)

{-# RULES
"curry apply"         curry apply = id
-- "curry . uncurry"     curry . uncurry = id
-- "uncurry . curry"     uncurry . curry = id
 #-}

-- * Free @'Adjunction' (Product (<=) a) (Exp (<=) a) (<=) (<=)@

-- unitCCC :: (CCC (<=), (<*>) ~ Product (<=), (<\>) ~ Exp (<=)) => a <= b <\> (b <*> a)
unitCCC :: CCC (<=) => a <= Exp (<=) b (Product (<=) b a)
unitCCC = curry braid

-- counitCCC :: (CCC (<=), (<*>) ~ Product (<=), (<\>) ~ Exp (<=)) => (b <*> (b <\> a)) <= a
counitCCC :: CCC (<=) => (Product (<=) b (Exp (<=) b a)) <= a
counitCCC = apply . braid

-- * A Co-(Closed Cartesian Category) 

-- | A Co-CCC has full-fledged comonoidal finite coproducts and coexponentials

-- You probably also want an instance for @'Bifunctor' ('coexp' hom) ('Dual' hom) hom hom@.

class 
    ( CoCartesian (<=)
    , Symmetric (<=) (Sum (<=))
    , Comonoidal (<=) (Sum (<=))
    ) => CoCCC (<=) where
    type Coexp (<=) :: * -> * -> *
    coapply :: b <= Sum (<=) (Coexp (<=) a b) a
    cocurry :: (c <= Sum (<=) a b) -> (Coexp (<=) b c <= a)
    uncocurry :: (Coexp (<=) b c <= a) -> (c <= Sum (<=) a b)

{-# RULES
"cocurry coapply"        cocurry coapply = id
-- "cocurry . uncocurry"   cocurry . uncocurry = id
-- "uncocurry . cocurry"   uncocurry . cocurry = id
 #-}

-- * Free @'Adjunction' ('Coexp' (<=) a) ('Sum' (<=) a) (<=) (<=)@
-- unitCoCCC :: (CoCCC (<=), subtract ~ Coexp (<=), (+) ~ Sum (<=)) => a <= b + subtract b a
unitCoCCC :: (CoCCC (<=)) => a <= Sum (<=) b (Coexp (<=) b a)
unitCoCCC = swap . coapply

counitCoCCC :: (CoCCC (<=), subtract ~ Coexp (<=), (+) ~ Sum (<=)) => subtract b (b + a) <= a
counitCoCCC = cocurry swap
