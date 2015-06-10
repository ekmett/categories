{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Math.Multicategory 
  ( Forest(..)
  , C(..)
  , Multicategory(..)
  -- * Utilities 
  , inputs, outputs
  , splitForest
  ) where

import Data.Constraint
import Data.Proxy
import Data.Type.Equality
import Math.Category
import Math.Functor
import Math.Monad
import Math.Polycategory.PRO
import Math.Rec
import Prelude (($))
import qualified Prelude



--------------------------------------------------------------------------------
-- * Forests
--------------------------------------------------------------------------------

data Forest :: ([i] -> i -> *) -> [i] -> [i] -> * where
  Nil :: Forest f '[] '[]
  (:-) :: f k o -> Forest f m n -> Forest f (k ++ m) (o ': n)

inputs  :: forall f g is os. (forall as b. f as b -> Rec g as) -> Forest f is os -> Rec g is
inputs _ Nil = RNil 
inputs f (a :- as) = f a `appendRec` inputs f as

outputs :: (forall as b. f as b -> p b) -> Forest f is os -> Rec p os
outputs _ Nil = RNil
outputs f (a :- as) = f a :& outputs f as

splitForest :: forall f g ds is js os r. Rec f is -> Forest g js os -> Forest g ds (is ++ js)
            -> (forall bs cs. (ds ~ (bs ++ cs)) => Forest g bs is -> Forest g cs js -> r) -> r
splitForest RNil _ as k = k Nil as
splitForest (_ :& is) bs ((j :: g as o) :- js) k = splitForest is bs js $
  \ (l :: Forest g bs as1) (r :: Forest g cs js) ->
    case appendAssocAxiom (Proxy :: Proxy as) (Proxy :: Proxy bs) (Proxy :: Proxy cs) of
      Dict -> k (j :- l) r

instance Multicategory f => Functor (Forest f) where
  type Dom (Forest f) = Op (Forest f)
  type Cod (Forest f) = Nat (Forest f) (->)
  fmap (Op f) = Nat (. f)

instance (Multicategory f) => Functor (Forest f is) where
  type Dom (Forest f is) = Forest f
  type Cod (Forest f is) = (->)
  fmap = (.)

--------------------------------------------------------------------------------
-- * Forgetting the multicategory structure
--------------------------------------------------------------------------------

data C (f :: [i] -> i -> *) (a :: i) (b :: i) where
  C :: { runC :: f '[a] b } -> C f a b

instance Multicategory f => Category (C f) where
  type Ob (C f) = Mob f
  id = C ident
  C f . C g = C (compose f (g :- Nil))
  source (C f) = case sources f of Dict1 :& RNil -> Dict
  target (C f) = case mtarget f of Dict1 -> Dict

--------------------------------------------------------------------------------
-- * Multicategories
--------------------------------------------------------------------------------


class
  ( Category (MDom f)
  , Bifunctor f
  , Dom f ~ Op (Forest f)
  , Dom2 f ~ C f
  , Cod2 f ~ (->)
  ) => Multicategory (f :: [i] -> i -> *) where
  type MDom f :: i -> i -> *
  ident   :: Mob f a => f '[a] a
  sources :: f as b -> Rec (Dict1 (Mob f)) as
  mtarget :: f as b -> Dict1 (Mob f) b

type Mob f = Ob (MDom f)

compose :: Multicategory f => f bs c -> Forest f as bs -> f as c
compose f es = case mtarget f of
  Dict1 -> lmap es f

instance Multicategory f => Category (Forest f) where
  type Ob (Forest f) = All (Mob f)

  id = go proofs where
    go :: Rec (Dict1 (Mob f)) is -> Forest f is is
    go (Dict1 :& as) = ident :- go as
    go RNil          = Nil

  Nil . Nil = Nil
  (b :- bs) . as = splitForest (sources b) bs as $ \es fs -> compose b es :- (bs . fs)

  source = reproof . inputs sources
  target = reproof . outputs mtarget

instance Multicategory f => PRO (Forest f) where
  pro Nil rs = rs
  pro (l :- ls) rs = case appendAssocAxiom (sources l) (go (source ls)) (go (source rs)) of
      Dict -> l :- pro ls rs 
    where
     go :: Dict (All p as) -> Rec (Dict1 p) as
     go Dict = proofs

data IM :: ([k] -> k -> *) -> (k -> *) -> k -> * where
  IM :: f is o -> Rec a is -> IM f a o

-- A subcategory of (:~:) satisfying a constraint `p`

data Dat p i j where
  Dat :: p i => Dat p i i

dum :: Dat p i j -> i :~: j
dum Dat = Refl

instance Category (Dat p) where
  type Ob (Dat p) = p
  id = Dat
  Dat . Dat = Dat
  source Dat{} = Dict
  target Dat{} = Dict

instance Functor (Dat p) where
  type Dom (Dat p) = Op (Dat p)
  type Cod (Dat p) = Nat (Dat p) (->)

instance Functor (Dat p a) where
  type Dom (Dat p a) = Dat p
  type Cod (Dat p a) = (->)

--instance Functor IM where
--  type Dom IM = Nat (:~:) (Nat (:~:) (->))
--  type Cod IM = Nat (Nat (:~:) (->)) (Nat (:~:) (->))

instance Multicategory f => Functor (IM f) where
  type Dom (IM f) = Nat (Dat (Mob f)) (->)
  type Cod (IM f) = Nat (Dat (Mob f)) (->)
  fmap f = Nat $ \(IM s d) -> IM s (runNat (fmap (go f)) d) where
    go :: Nat (Dat (Mob f)) (->) i j -> Nat (:~:) (->) i j
    go = Prelude.undefined -- TODO
    -- what we really need is a restricted 'Rec' which can only be mapped with a Dat (Mob f) rather than (:~:)

instance Multicategory f => Functor (IM f a) where
  type Dom (IM f a) = Dat (Mob f)
  type Cod (IM f a) = (->)
  fmap Dat a = a

instance Multicategory f => Monad (IM f) where
  return = Nat $ \a -> IM ident (a :& RNil)
