{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- really, GHC, really?
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Math.Category.Sum
  ( type (+)(..)
  , sumOb
  ) where

import Prelude (Either(..))
import Data.Constraint
import Data.Proxy
import Data.Kind(Type)
import Math.Category
import Math.Groupoid

type Left = 'Left
type Right = 'Right

class SumOb (p :: i -> i -> Type) (q :: j -> j -> Type) (o :: Either i j) where
  sumOb :: proxy1 p -> proxy2 q -> proxy3 o ->
    (forall a. Ob p a => (o ~ Left a) => r) -> (forall b. Ob q b => (o ~ Right b) => r) -> r

instance Ob p a => SumOb p q (Left a) where
  sumOb _ _ _ l _ = l

instance Ob q b => SumOb p q (Right b) where
  sumOb _ _ _ _ r = r

data (+) :: (i -> i -> Type) -> (j -> j -> Type) -> Either i j -> Either i j -> Type where
  L :: p a b -> (p + q) (Left a) (Left b)
  R :: q a b -> (p + q) (Right a) (Right b)

instance (Category p, Category q) => Category (p + q) where
  type Ob (p + q) = SumOb p q
  id = it where
    it :: forall o. SumOb p q o => (p + q) o o
    it = sumOb (Proxy :: Proxy p) (Proxy :: Proxy q) (Proxy :: Proxy o) (L id) (R id)

  L f . L g = L (f . g)
  R f . R g = R (f . g)

  source (L p) = case source p of Dict -> Dict
  source (R q) = case source q of Dict -> Dict

  target (L p) = case target p of Dict -> Dict
  target (R q) = case target q of Dict -> Dict

instance (Groupoid p, Groupoid q) => Groupoid (p + q) where
  inv (L f) = L (inv f)
  inv (R g) = R (inv g)
