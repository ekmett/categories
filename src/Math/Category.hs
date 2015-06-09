{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Math.Category 
  ( Category(..)
  , Vacuous
  ) where

import Data.Constraint    as Constraint
import Data.Type.Equality as Equality
import Data.Type.Coercion as Coercion

class Vacuous (a :: i)
instance Vacuous a

class Category (p :: i -> i -> *) where
  type Ob p :: i -> Constraint
  type Ob p = Vacuous

  id :: Ob p a => p a a
  (.) :: p b c -> p a b -> p a c

  source :: p a b -> Dict (Ob p a)
  default source :: (Ob p ~ Vacuous) => p a b -> Dict (Ob p a)
  source _ = Dict

  target :: p a b -> Dict (Ob p b)
  default target :: (Ob p ~ Vacuous) => p a b -> Dict (Ob p b)
  target _ = Dict

instance Category (->) where
  id = Prelude.id
  (.) = (Prelude..)

instance Category (:-) where
  id = Constraint.refl
  (.) = Constraint.trans

instance Category (:~:) where
  id = Equality.Refl
  (.) = Prelude.flip Equality.trans

instance Category Coercion where
  id = Coercion
  (.) = Prelude.flip Coercion.trans
