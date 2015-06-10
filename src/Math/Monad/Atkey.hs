{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}

module Math.Monad.Atkey 
  ( At(..)
  , Atkey
  , Coat(..)
  , Coatkey
  ) where

import Data.Type.Equality
import Math.Category
import Math.Functor
import Prelude (($))

data At a i j where
  At :: a -> At a i i

instance Functor (At a i) where
  type Dom (At a i) = (:~:)
  type Cod (At a i) = (->)
  fmap Refl a = a

instance Functor (At a) where
  type Dom (At a) = (:~:)
  type Cod (At a) = Nat (:~:) (->)
  fmap Refl = Nat id

instance Functor At where
  type Dom At = (->)
  type Cod At = Nat (:~:) (Nat (:~:) (->))
  fmap f = Nat $ Nat $ \(At a) -> At (f a)

type Atkey m i j a = m (At a j) i

newtype Coat a i j = Coat { runCoat :: (i ~ j) => a }

instance Functor (Coat a i) where
  type Dom (Coat a i) = (:~:)
  type Cod (Coat a i) = (->)
  fmap Refl a = a

instance Functor (Coat a) where
  type Dom (Coat a) = (:~:)
  type Cod (Coat a) = Nat (:~:) (->)
  fmap Refl = Nat id

instance Functor Coat where
  type Dom Coat = (->)
  type Cod Coat = Nat (:~:) (Nat (:~:) (->))
  fmap f = Nat $ Nat $ \ g -> Coat (f (runCoat g))

type Coatkey m i j a = m (Coat a j) i
