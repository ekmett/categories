{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Math.Operad where

import Data.Constraint
import Data.Proxy
import Data.Type.Equality
import Math.Category
import Math.Functor
import Math.Monad
import Math.Monad.Atkey
import Math.Multicategory
import Math.Polycategory.PRO
import Math.Rec
import Prelude (($))

class (Multicategory f, Mob f ~ (~) '()) => Operad f
instance (Multicategory f, Mob f ~ (~) '()) => Operad f

-- the monad associated with an operad
data M (f :: [()] -> () -> *) (a :: *) where
  M :: f n '() -> Rec (At a '()) n -> M f a

--instance Functor M where
--  type Dom M = Nat (:~:) (Nat (:~:) (:~:))
--  type Cod M = Nat (->) (->)

instance Functor (M f) where
  type Dom (M f) = (->)
  type Cod (M f) = (->)
  fmap f (M s d) = M s (mapRec (at f) d)


instance Operad f => Monad (M f) where
  return a = M ident (At a :& RNil)
  bind (f :: a -> M f b) (M s0 d0) = go d0 $ \ as ds -> M (compose s0 as) ds where
    go :: Rec (At a '()) is -> (forall os. Forest f os is -> Rec (At b '()) os -> r) -> r
    go RNil k = k Nil RNil
    go (At a :& is) k = go is $ \fs as -> case f a of
      M s bs -> k (s :- fs) (appendRec bs as)

-- | The comonad associated with an operad
newtype W (f :: [()] -> () -> *) (a :: *) = W { runW :: forall is. f is '() -> Rec (Coat a '()) is }

instance Functor (W f) where
  type Dom (W f) = (->)
  type Cod (W f) = (->)
  fmap f (W g) = W (mapRec (\(Coat a) -> Coat (f a)) . g)

instance Operad f => Comonad (W f) where
  extract (W f) = case f ident of
    Coat a :& RNil -> a
  extend (f :: W f a -> b) (w :: W f a) = W $ \s -> go RNil (sources s) s where
    go :: forall (ls :: [()]) (rs :: [()]). Rec (Dict1 (Mob f)) ls -> Rec (Dict1 (Mob f)) rs -> f (ls ++ rs) '() -> Rec (Coat b '()) rs
    go _ RNil _ = RNil
    go ls (p :& rs) s = g :& go (appendRec ls (p :& RNil)) rs (shift s)
      where
        g = Coat $ f $ W $ \s' ->
          prune ls (sources s') rs (runW w (compose s (pro (idents ls) (s' :- idents rs))))
        prune ls is rs = takeRec is rs . dropRec ls
        shift s = case appendAssocAxiom ls (p :& RNil) rs of Dict -> s
        idents :: Rec (Dict1 (Mob f)) as -> Forest f as as
        idents p = case reproof p of Dict -> id
