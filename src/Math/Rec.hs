{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Trustworthy #-}

-- GHC warns about splitRec, but a "fix" yield unreachable code and won't compile.
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Math.Rec
  ( Rec(..)
  , appendRec
  , mapRec
  , splitRec
  , takeRec
  , dropRec
  , foldrRec
  , traverseRec
  -- * Type Lists
  , type (++)
  , appendNilAxiom
  , appendAssocAxiom
  -- * Dict1
  , Dict1(..)
  -- * All
  , All(..)
  , reproof
  ) where

import Control.Applicative
import Data.Constraint
import Data.Type.Equality
import Math.Functor
import Prelude (($), fst, snd)
import Unsafe.Coerce

type family (++) (a :: [k]) (b :: [k]) :: [k]
type instance '[] ++ bs = bs
type instance (a ': as) ++ bs = a ': (as ++ bs)

-- | Proof provided by every single class on theorem proving in the last 20 years.
appendNilAxiom :: forall as. Dict (as ~ (as ++ '[]))
appendNilAxiom = unsafeCoerce (Dict :: Dict (as ~ as))

-- | Proof provided by every single class on theorem proving in the last 20 years.
appendAssocAxiom :: forall p q r as bs cs. p as -> q bs -> r cs -> Dict ((as ++ (bs ++ cs)) ~ ((as ++ bs) ++ cs))
appendAssocAxiom _ _ _ = unsafeCoerce (Dict :: Dict (as ~ as))

data Rec f as where
  RNil :: Rec f '[]
  (:&) :: !(f i) -> !(Rec f is) -> Rec f (i ': is)

instance Functor (Rec f) where
  type Dom (Rec f) = (:~:)
  type Cod (Rec f) = (->)
  fmap Refl as = as

instance Functor Rec where
  type Dom Rec = Nat (:~:) (->)
  type Cod Rec = Nat (:~:) (->)
  fmap f = Nat $ mapRec (runNat f)

-- | Append two records
appendRec :: Rec f as -> Rec f bs -> Rec f (as ++ bs)
appendRec RNil bs      = bs
appendRec (a :& as) bs = a :& appendRec as bs

-- | Map over a record
mapRec :: (forall a. f a -> g a) -> Rec f as -> Rec g as
mapRec _ RNil = RNil
mapRec f (a :& as) = f a :& mapRec f as

-- | Split a record
splitRec :: Rec f is -> Rec g (is ++ js) -> (Rec g is, Rec g js)
splitRec (_ :& is) (a :& as) = case splitRec is as of
  (l,r) -> (a :& l, r)
splitRec RNil    as    = (RNil, as)
-- splitRec (_ :& _) RNil = error "splitRec: the impossible happened"

takeRec :: forall f g h is js. Rec f is -> Rec g js -> Rec h (is ++ js) -> Rec h is
takeRec is _ ijs = fst $ (splitRec is ijs :: (Rec h is, Rec h js))

dropRec :: Rec f is -> Rec g (is ++ js) -> Rec g js
dropRec is ijs = snd $ splitRec is ijs

foldrRec :: (forall j js. f j -> r js -> r (j ': js)) -> r '[] -> Rec f is -> r is
foldrRec _ z RNil = z
foldrRec f z (a :& as) = f a (foldrRec f z as)

traverseRec :: Applicative m => (forall i. f i -> m (g i)) -> Rec f is -> m (Rec g is)
traverseRec f (a :& as) = (:&) <$> f a <*> traverseRec f as
traverseRec _ RNil = pure RNil

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
