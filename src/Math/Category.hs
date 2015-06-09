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
  , Yoneda(..)
  , Op
  , Vacuous
  ) where

import Data.Constraint    as Constraint
import Data.Type.Equality as Equality
import Data.Type.Coercion as Coercion
import qualified Prelude

-- | The <http://ncatlab.org/nlab/show/Yoneda+embedding Yoneda embedding>.
--
-- Yoneda_C :: C -> [ C^op, Set ]
newtype Yoneda (p :: i -> i -> *) (a :: i) (b :: i) = Op { getOp :: p b a }

type family Op (p :: i -> i -> *) :: i -> i -> * where
  Op (Yoneda p) = p
  Op p = Yoneda p

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

  unop :: Op p b a -> p a b
  default unop :: Op p ~ Yoneda p => Op p b a -> p a b
  unop = getOp

  op :: p b a -> Op p a b
  default op :: Op p ~ Yoneda p => p b a -> Op p a b
  op = Op

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

instance (Category p, Op p ~ Yoneda p) => Category (Yoneda p) where
  type Ob (Yoneda p) = Ob p
  id = Op id
  Op f . Op g = Op (g . f)
  source (Op f) = target f
  target (Op f) = source f
  unop = Op
  op = getOp
