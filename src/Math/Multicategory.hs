{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
module Math.Multicategory where

import Data.Constraint
import Math.Category
import Math.Rec
import Math.Rec.All

data Forest :: ([i] -> i -> *) -> [i] -> [i] -> * where
  Nil :: Forest f '[] '[]
  Cons :: f k o -> Forest f m n -> Forest f (k ++ m) (o ': n)

inputs  :: forall f g is os. (forall as b. f as b -> Rec g as) -> Forest f is os -> Rec g is
inputs _ Nil = RNil 
inputs f (Cons a as) = f a `appendRec` inputs f as

outputs :: (forall as b. f as b -> p b) -> Forest f is os -> Rec p os
outputs f Nil = RNil
outputs f (Cons a as) = f a :& outputs f as

class Multicategory (f :: [i] -> i -> *) where
  type Mob f :: i -> Constraint
  type Mob f = Vacuous
  ident    :: Mob f a => f '[a] a
  compose  :: f bs c -> Forest f as bs -> f as c
  msources :: f as b -> Rec (Dict1 (Mob f)) as
  mtarget  :: f as b -> Dict (Mob f b)

instance Multicategory f => Category (Forest f) where
  type Ob (Forest f) = All (Mob f)
  id = undefined -- Cons ident Nil
  (.) = undefined
  source = undefined
  target = undefined
