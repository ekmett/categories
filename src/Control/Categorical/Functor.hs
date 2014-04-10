{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

-------------------------------------------------------------------------------------------
-- |
-- Module      : Control.Categorical.Functor
-- Copyright   : 2008-2014 Edward Kmett
-- License     : BSD3
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (functional-dependencies)
--
-- A more categorical definition of 'Functor'
-------------------------------------------------------------------------------------------
module Control.Categorical.Functor
  ( Functor(fmap)
  , Endofunctor
  , LiftedFunctor(..)
  , LoweredFunctor(..)
  , LiftedContravariant(..)
  , LoweredContravariant(..)
  ) where


import Control.Category
import Control.Category.Dual
import Data.Data (Data, Typeable)
import qualified Data.Functor.Contravariant as Contravariant
import GHC.Generics
import Prelude hiding (id, (.), Functor(..))
import qualified Prelude as Prelude

newtype LiftedFunctor f (a :: *) = LiftedFunctor (f a)
  deriving (Eq,Ord,Show,Read,Typeable,Generic)

deriving instance (Typeable f, Data (f a), Data a) => Data (LiftedFunctor f a)

newtype LoweredFunctor f (a :: *) = LoweredFunctor (f a)
  deriving (Eq,Ord,Show,Read,Typeable,Generic)

deriving instance (Typeable f, Data (f a), Data a) => Data (LoweredFunctor f a)

newtype LiftedContravariant f (a :: *) = LiftedContravariant (f a)
  deriving (Eq,Ord,Show,Read,Typeable,Generic)

deriving instance (Typeable f, Data (f a), Data a) => Data (LiftedContravariant f a)

newtype LoweredContravariant f (a :: *) = LoweredContravariant (f a)
  deriving (Eq,Ord,Show,Read,Typeable,Generic)

deriving instance (Typeable f, Data (f a), Data a) => Data (LoweredContravariant f a)

class (Category r, Category t) => Functor (f :: x -> y) (r :: x -> x -> *) (t :: y -> y -> *) | f r -> t, f t -> r where
  {-# MINIMAL fmap #-}
  fmap :: r a b -> t (f a) (f b)

  contramap :: (r ~ Dual r') => r' b a -> t (f a) (f b)
  contramap f = fmap (Dual f)

instance Functor f s t => Functor f (Dual (Dual s)) (Dual (Dual t)) where
  fmap (Dual (Dual f)) = Dual (Dual (fmap f))

-- I would hereby declare that any orphan instances of this class are unsafe
-- and proceed via overlapping instances. _However_ that doesn't work with MPTCs + fundeps

-- instance Prelude.Functor f => Functor f (->) (->) where
--   fmap = Prelude.fmap

instance Functor f (->) (->) => Prelude.Functor (LoweredFunctor f) where
  fmap f (LoweredFunctor a) = LoweredFunctor (fmap f a)

instance Prelude.Functor f => Functor (LiftedFunctor f) (->) (->) where
  fmap f (LiftedFunctor a) = LiftedFunctor (Prelude.fmap f a)

instance Prelude.Functor f => Functor (LiftedFunctor f) (Dual (->)) (Dual (->)) where
  fmap (Dual f) = Dual $ \(LiftedFunctor a) -> LiftedFunctor (Prelude.fmap f a)

instance Functor f (Dual (->)) (->) => Contravariant.Contravariant (LoweredContravariant f) where
  contramap f (LoweredContravariant a) = LoweredContravariant (contramap f a)

instance Contravariant.Contravariant f => Functor (LiftedContravariant f) (Dual (->)) (->) where
  fmap (Dual f) (LiftedContravariant a) = LiftedContravariant (Contravariant.contramap f a)

instance Contravariant.Contravariant f => Functor (LiftedContravariant f) (->) (Dual (->)) where
  fmap f = Dual $ \(LiftedContravariant a) -> LiftedContravariant (Contravariant.contramap f a)

instance Functor ((,) a) (->) (->) where
  fmap f (a, b) = (a, f b)

instance Functor (Either a) (->) (->) where
  fmap _ (Left a) = Left a
  fmap f (Right a) = Right (f a)

instance Functor Maybe (->) (->) where
  fmap = Prelude.fmap

instance Functor [] (->) (->) where
  fmap = Prelude.fmap

instance Functor IO (->) (->) where
  fmap = Prelude.fmap

class Functor f a a => Endofunctor f a
instance Functor f a a => Endofunctor f a
