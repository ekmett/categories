{-# LANGUAGE GADTs #-}
-------------------------------------------------------------------------------------------
-- |
-- Module    : Control.Category.Discrete
-- Copyright : 2008-2010 Edward Kmett
-- License   : BSD
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-------------------------------------------------------------------------------------------
module Control.Category.Discrete
    ( Discrete(Refl)
    , liftDiscrete
    , cast
    , inverse
    ) where

import Prelude hiding (id, (.))
import Control.Category
-- import Unsafe.Coerce (unsafeCoerce)

-- | Category of discrete objects. The only arrows are identity arrows.
data Discrete a b where 
    Refl :: Discrete a a

instance Category Discrete where
    id = Refl
    Refl . Refl = Refl

-- | Discrete a b acts as a proof that a = b, lift that proof into something of kind * -> *
liftDiscrete :: Discrete a b -> Discrete (f a) (f b)
liftDiscrete Refl = Refl

-- | Lower the proof that a ~ b to an arbitrary category.
cast :: Category (~>) => Discrete a b -> (a ~> b)
cast Refl = id

-- | 
inverse :: Discrete a b -> Discrete b a
inverse Refl = Refl
