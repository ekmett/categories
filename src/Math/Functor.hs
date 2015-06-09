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

module Math.Functor 
  ( Functor(..)
  , FunctorOf
  , Nat(..)
  ) where

import Data.Constraint as Constraint
import Data.Type.Equality as Equality
import Data.Type.Coercion as Coercion
import Math.Category
import Math.Category.Dual
import Prelude (($))

class (Category (Cod f), Category (Dom f)) => Functor (f :: i -> j) where
  type Dom f :: i -> i -> *
  type Cod f :: j -> j -> *
  fmap :: Dom f a b -> Cod f (f a) (f b)

instance Functor (->) where
  type Dom (->) = Dual (->)
  type Cod (->) = Nat (->) (->)
  fmap (Dual f) = Nat (. f)

instance Functor ((->) e) where
  type Dom ((->) e) = (->)
  type Cod ((->) e) = (->)
  fmap = (.)

instance Functor (:-) where
  type Dom (:-) = Dual (:-)
  type Cod (:-) = Nat (:-) (->)
  fmap (Dual f) = Nat (. f)

instance Functor ((:-) e) where
  type Dom ((:-) e) = (:-)
  type Cod ((:-) e) = (->)
  fmap = (.)

instance Functor Dict where
  type Dom Dict = (:-)
  type Cod Dict = (->)
  fmap p Dict = case p of
    Sub q -> q

instance Functor (:~:) where
  type Dom (:~:) = Dual (:~:)
  type Cod (:~:) = Nat (:~:) (->)
  fmap (Dual f) = Nat (. f)

instance Functor ((:~:) e) where
  type Dom ((:~:) e) = (:~:)
  type Cod ((:~:) e) = (->)
  fmap = (.)

instance Functor Coercion where
  type Dom Coercion = Dual Coercion
  type Cod Coercion = Nat Coercion (->)
  fmap (Dual f) = Nat (. f)

instance Functor (Coercion e) where
  type Dom (Coercion e) = Coercion
  type Cod (Coercion e) = (->)
  fmap = (.)

class (Dom f ~ c, Cod f ~ d, Functor f) => FunctorOf (c :: i -> i -> *) (d :: j -> j -> *) (f :: i -> j)
instance (Dom f ~ c, Cod f ~ d, Functor f) => FunctorOf c d f 

ob :: forall f a. Functor f => Ob (Dom f) a :- Ob (Cod f) (f a)
ob = Sub $ case source (fmap (id :: Dom f a a) :: Cod f (f a) (f a)) of
  Dict -> Dict

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
  type Dom (Nat c d) = Dual (Nat c d)
  type Cod (Nat c d) = Nat (Nat c d) (->)
  fmap (Dual f) = Nat (. f)
  
instance (Category c, Category d) => Functor (Nat c d f) where
  type Dom (Nat c d f) = Nat c d
  type Cod (Nat c d f) = (->)
  fmap = (.)

instance (Category c, Category d) => Functor (FunctorOf c d) where
  type Dom (FunctorOf c d) = Nat c d
  type Cod (FunctorOf c d) = (:-)
  fmap Nat{} = Sub Dict



