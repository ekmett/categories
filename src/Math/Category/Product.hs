{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Math.Category.Product
  ( Fst, Snd, ProductOb
  , (*)(..)
  ) where

import Data.Constraint
import Math.Category
import Math.Groupoid

type family Fst (r :: (a,b)) :: a where Fst '(x,y) = x
type family Snd (r :: (a,b)) :: b where Snd '(x,y) = y

class    (r ~ '(Fst r, Snd r), Ob p (Fst r), Ob q (Snd r)) => ProductOb p q r
instance (r ~ '(Fst r, Snd r), Ob p (Fst r), Ob q (Snd r)) => ProductOb p q r

data (*) :: (i -> i -> *) -> (j -> j -> *) -> (i,j) -> (i,j) -> * where
  Pair :: p a b -> q c d -> (p * q) '(a,c) '(b,d)

instance (Category p, Category q) => Category (p * q) where
  type Ob (p * q) = ProductOb p q
  id = Pair id id
  Pair f g . Pair h i = Pair (f . h) (g . i)
  source (Pair f g) = case source f of
    Dict -> case source g of
      Dict -> Dict
  target (Pair f g) = case target f of
    Dict -> case target g of
      Dict -> Dict

instance (Groupoid p, Groupoid q) => Groupoid (p * q) where
  inv (Pair p q) = Pair (inv p) (inv q)
