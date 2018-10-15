{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-unused-imports #-}

module Math.Multicategory.Free
  ( Graded(..)
  , Free(..)
  ) where

import Data.Constraint
import Data.Proxy
import Math.Category
import Math.Multicategory
import Math.Rec
import Prelude (const)

class Graded f where
  grade :: f is o -> Rec Proxy is

data Free :: ([i] -> i -> *) -> [i] -> i -> * where
  Ident :: Free f '[a] a
  Apply :: f bs c -> Forest (Free f) as bs -> Free f as c

instance Graded f => Graded (Free f) where
  grade Ident        = Proxy :& RNil
  grade (Apply _ as) = inputs grade as

#if 0  
instance Graded f => Multicategory (Free f) where
  type Mob (Free f) = Vacuous
  ident = Ident
  compose Ident ((a :: Free f bs c) :- Nil) = case appendNilAxiom :: Dict (bs ~ (bs ++ '[])) of Dict -> a
  compose (Apply f as) bs = Apply f (as . bs)

  sources m = mapRec (const Dict1) (grade m)

  mtarget _ = Dict1
#endif