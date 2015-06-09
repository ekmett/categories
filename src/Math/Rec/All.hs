{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Math.Rec.All
  ( Dict1(..)
  , All(..)
  , reproof
  ) where

import Data.Constraint
import Math.Rec

data Dict1 p a where
  Dict1 :: p a => Dict1 p a

class All (p :: i -> Constraint) (is :: [i]) where
  proofs :: Rec (Dict1 p) is

instance All p '[] where
  proofs = RNil

instance (p i, All p is) => All p (i ': is) where
  proofs = Dict1 :& proofs

reproof :: Rec (Dict1 p) is -> Dict (All p is)
reproof RNil = Dict
reproof (Dict1 :& as) = case reproof as of
  Dict -> Dict
