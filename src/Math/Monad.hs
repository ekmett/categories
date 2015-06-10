{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Math.Monad
  ( Monad(..)
  , Comonad(..)
  ) where

import Data.Constraint
import Math.Category
import Math.Functor
import qualified Control.Monad as Base
import qualified Prelude

class (Functor f, Cod f ~ Dom f) => Monad f where
  return :: Ob (Dom f) a => Dom f a (f a)
  join :: forall a. Ob (Dom f) a => Dom f (f (f a)) (f a)
  join = bind (id \\ (ob :: Ob (Dom f) a :- Ob (Cod f) (f a)))
  bind :: Ob (Dom f) b => Dom f a (f b) -> Dom f (f a) (f b)
  bind f = join . fmap f

instance Monad [] where
  return = Base.return
  join = Base.join

instance Monad Prelude.Maybe where
  return = Base.return
  join = Base.join

instance Monad (Prelude.Either a) where
  return = Base.return
  join = Base.join

instance Monad ((->) e) where
  return = Base.return
  join = Base.join

class (Functor f, Cod f ~ Dom f) => Comonad f where
  extract :: Ob (Dom f) a => Dom f (f a) a
  duplicate :: forall a. Ob (Dom f) a => Dom f (f a) (f (f a))
  duplicate = extend (id \\ (ob :: Ob (Dom f) a :- Ob (Cod f) (f a)))
  extend :: Ob (Dom f) a => Dom f (f a) b -> Dom f (f a) (f b)
  extend f = fmap f . duplicate

instance Comonad ((,) e) where
  extract (_, a) = a
  duplicate ea@(e, _) = (e, ea)
  extend f ea@(e, _) = (e, f ea)
