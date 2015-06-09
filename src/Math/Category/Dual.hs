{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Math.Category.Dual 
  ( Dual(..)
  ) where

import Math.Category
import Math.Groupoid

newtype Dual p a b = Dual { runDual :: p b a }

instance Category p => Category (Dual p) where
  type Ob (Dual p) = Ob p
  id = Dual id
  Dual f . Dual g = Dual (g . f)
  source (Dual f) = target f
  target (Dual f) = source f

instance Groupoid p => Groupoid (Dual p) where
  inv (Dual f) = Dual (inv f)
