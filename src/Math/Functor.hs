{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Math.Functor 
  ( Functor(..)
  , FunctorOf
  , Nat(..)
  , Bifunctor, Dom2, Cod2
  , bimap, first, second
  , dimap
  , contramap
  ) where

import Data.Constraint as Constraint
import Data.Type.Equality as Equality
import Data.Type.Coercion as Coercion
import Math.Category
import Prelude (($), Either(..))

--------------------------------------------------------------------------------
-- * Functors
--------------------------------------------------------------------------------

class (Category (Cod f), Category (Dom f)) => Functor (f :: i -> j) where
  type Dom f :: i -> i -> *
  type Cod f :: j -> j -> *
  fmap :: Dom f a b -> Cod f (f a) (f b)

contramap :: Functor f => Op (Dom f) b a -> Cod f (f a) (f b)
contramap = fmap . unop

instance Functor ((->) e) where
  type Dom ((->) e) = (->)
  type Cod ((->) e) = (->)
  fmap = (.)

instance Functor ((:-) e) where
  type Dom ((:-) e) = (:-)
  type Cod ((:-) e) = (->)
  fmap = (.)

instance Functor Dict where
  type Dom Dict = (:-)
  type Cod Dict = (->)
  fmap p Dict = case p of
    Sub q -> q

instance Functor ((:~:) e) where
  type Dom ((:~:) e) = (:~:)
  type Cod ((:~:) e) = (->)
  fmap = (.)

instance Functor (Coercion e) where
  type Dom (Coercion e) = Coercion
  type Cod (Coercion e) = (->)
  fmap = (.)

instance Functor ((,) e) where
  type Dom ((,) e) = (->)
  type Cod ((,) e) = (->)
  fmap f ~(a,b) = (a, f b)

instance Functor (Either a) where
  type Dom (Either a) = (->)
  type Cod (Either a) = (->)
  fmap _ (Left a) = Left a
  fmap f (Right b) = Right (f b)

class (Dom f ~ c, Cod f ~ d, Functor f) => FunctorOf (c :: i -> i -> *) (d :: j -> j -> *) (f :: i -> j)
instance (Dom f ~ c, Cod f ~ d, Functor f) => FunctorOf c d f 

ob :: forall f a. Functor f => Ob (Dom f) a :- Ob (Cod f) (f a)
ob = Sub $ case source (fmap (id :: Dom f a a) :: Cod f (f a) (f a)) of
  Dict -> Dict

--------------------------------------------------------------------------------
-- * Natural Transformations
--------------------------------------------------------------------------------

data Nat (c :: i -> i -> *) (d :: j -> j -> *) (f :: i -> j) (g :: i -> j) where
  Nat :: (FunctorOf c d f, FunctorOf c d g) => { runNat :: forall a. Ob c a => d (f a) (g a) }  -> Nat c d f g

instance (Category c, Category d) => Category (Nat c d) where
  type Ob (Nat c d) = FunctorOf c d
  id = Nat id1 where
    id1 :: forall f x. (Functor f, Dom f ~ c, Cod f ~ d, Ob c x) => d (f x) (f x)
    id1 = id \\ (ob :: Ob c x :- Ob d (f x))
  Nat f . Nat g = Nat (f . g)
  source Nat{} = Dict
  target Nat{} = Dict

instance (Category c, Category d) => Functor (Nat c d) where
  type Dom (Nat c d) = Op (Nat c d)
  type Cod (Nat c d) = Nat (Nat c d) (->)
  fmap (Op f) = Nat (. f)
  
instance (Category c, Category d) => Functor (Nat c d f) where
  type Dom (Nat c d f) = Nat c d
  type Cod (Nat c d f) = (->)
  fmap = (.)

instance (Category c, Category d) => Functor (FunctorOf c d) where
  type Dom (FunctorOf c d) = Nat c d
  type Cod (FunctorOf c d) = (:-)
  fmap Nat{} = Sub Dict

instance Functor (->) where
  type Dom (->) = Op (->)
  type Cod (->) = Nat (->) (->)
  fmap (Op f) = Nat (. f)

instance Functor (:-) where
  type Dom (:-) = Op (:-)
  type Cod (:-) = Nat (:-) (->)
  fmap (Op f) = Nat (. f)

instance Functor (:~:) where
  type Dom (:~:) = Op (:~:)
  type Cod (:~:) = Nat (:~:) (->)
  fmap (Op f) = Nat (. f)

instance Functor Coercion where
  type Dom Coercion = Op Coercion
  type Cod Coercion = Nat Coercion (->)
  fmap (Op f) = Nat (. f)

instance Functor (,) where
  type Dom (,) = (->)
  type Cod (,) = Nat (->) (->)
  fmap f = Nat $ \(a,b) -> (f a, b)

instance Functor Either where
  type Dom Either = (->)
  type Cod Either = Nat (->) (->)
  fmap f0 = Nat (go f0) where
    go :: (a -> b) -> Either a c -> Either b c
    go f (Left a)  = Left (f a)
    go _ (Right b) = Right b

--------------------------------------------------------------------------------
-- * Bifunctors
--------------------------------------------------------------------------------

type family NatDom (f :: (i -> j) -> (i -> j) -> *) :: (i -> i -> *) where NatDom (Nat p q) = p
type family NatCod (f :: (i -> j) -> (i -> j) -> *) :: (j -> j -> *) where NatCod (Nat p q) = q

type Dom2 p = NatDom (Cod p)
type Cod2 p = NatCod (Cod p)

class (Functor p, Cod p ~ Nat (Dom2 p) (Cod2 p), Category (Dom2 p), Category (Cod2 p)) => Bifunctor (p :: i -> j -> k)
instance  (Functor p, Cod p ~ Nat (Dom2 p) (Cod2 p), Category (Dom2 p), Category (Cod2 p)) => Bifunctor (p :: i -> j -> k)

first :: (Functor f, Cod f ~ Nat d e, Ob d c) => Dom f a b -> e (f a c) (f b c)
first = runNat . fmap

second :: forall p a b c. (Bifunctor p, Ob (Dom p) c) => Dom2 p a b -> Cod2 p (p c a) (p c b)
second f = case ob :: Ob (Dom p) c :- FunctorOf (Dom2 p) (Cod2 p) (p c) of
  Sub Dict -> fmap f

bimap :: Bifunctor p => Dom p a b -> Dom2 p c d -> Cod2 p (p a c) (p b d)
bimap f g = case source f of
  Dict -> case target g of
    Dict -> runNat (fmap f) . second g

-- mapping over a profunctor 
dimap :: Bifunctor p => Op (Dom p) b a -> Dom2 p c d -> Cod2 p (p a c) (p b d)
dimap = bimap . unop
