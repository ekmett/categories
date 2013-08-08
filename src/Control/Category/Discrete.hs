{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------------------------------

-- |
-- Module    : Control.Category.Discrete
-- Copyright : 2008-2013 Edward Kmett
-- License   : BSD
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-------------------------------------------------------------------------------------------
module Control.Category.Discrete
    ( (==)(Refl)
    , liftDiscrete
    , cast
    , inverse
    ) where

import Prelude ()
import Control.Category

infixr 0 ==

-- | Category of discrete objects. The only arrows are identity arrows.
data (==) :: x -> x -> * where
    Refl :: a == a

instance Category (==) where
    id = Refl
    Refl . Refl = Refl

-- | Discrete a b acts as a proof that a = b, lift that proof into something of kind * -> *
liftDiscrete :: (a == b) -> f a == f b
liftDiscrete Refl = Refl

-- | Lower the proof that a ~ b to an arbitrary category.
cast :: Category k => (a == b) -> k a b
cast Refl = id

-- |
inverse :: (a == b) -> b == a
inverse Refl = Refl
