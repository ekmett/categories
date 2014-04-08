{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE DeriveDataTypeable #-}
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
  ) where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

import Control.Categorical.Category
import qualified Data.Functor.Contravariant as Contravariant
import Prelude hiding (id, (.), Functor(..))
import qualified Prelude as Prelude
import Data.Data (Data(..), mkDataType, DataType, mkConstr, Constr, constrIndex, Fixity(..))
#if __GLASGOW_HASKELL__ >= 707
#define Typeable1 Typeable
import Data.Typeable (Typeable, gcast1)
#elif __GLASGOW_HASKELL__ >= 702
import Data.Typeable (Typeable1(..), TyCon, mkTyCon3, mkTyConApp, gcast1)
#else
import Data.Typeable (Typeable1(..), TyCon, mkTyCon, mkTyConApp, gcast1)
#endif

newtype LiftedFunctor f a = LiftedFunctor (f a)
  deriving
  ( Show
  , Read
#if __GLASGOW_HASKELL__ >= 707
  , Typeable
#endif
  )

newtype LoweredFunctor f a = LoweredFunctor (f a)
  deriving
  ( Show
  , Read
#if __GLASGOW_HASKELL__ >= 707
  , Typeable
#endif
  )

newtype LiftedContravariant f a = LiftedContravariant (f a)
  deriving
  ( Show
  , Read
#if __GLASGOW_HASKELL__ >= 707
  , Typeable
#endif
  )

newtype LoweredContravariant f a = LoweredContravariant (f a)
  deriving
  ( Show
  , Read
#if __GLASGOW_HASKELL__ >= 707
  , Typeable
#endif
  )


#if __GLASGOW_HASKELL__ < 707

liftedTyCon, loweredTyCon, liftedContraTyCon, loweredContraTyCon  :: TyCon
#if __GLASGOW_HASKELL__ >= 702
liftedTyCon = mkTyCon3 "categories" "Control.Categorical.Functor" "LiftedFunctor"
loweredTyCon = mkTyCon3 "categories" "Control.Categorical.Functor" "LoweredFunctor"
liftedContraTyCon = mkTyCon3 "categories" "Control.Categorical.Functor" "LiftedContravariant"
loweredContraTyCon = mkTyCon3 "categories" "Control.Categorical.Functor" "LoweredContravariant"
#else
liftedTyCon = mkTyCon "Control.Categorical.Functor.LiftedFunctor"
loweredTyCon = mkTyCon "Control.Categorical.Functor.LoweredFunctor"
liftedContraTyCon = mkTyCon "Control.Categorical.Functor.LiftedContravariant"
loweredContraTyCon = mkTyCon "Control.Categorical.Functor.LoweredContravariant"
#endif
{-# NOINLINE loweredTyCon #-}
{-# NOINLINE liftedTyCon #-}
{-# NOINLINE loweredContraTyCon #-}
{-# NOINLINE liftedContraTyCon #-}

instance Typeable1 f => Typeable1 (LiftedFunctor f) where
  typeOf1 tfa = mkTyConApp liftedTyCon [typeOf1 (undefined `asArgsType` tfa)]
    where asArgsType :: f a -> t f a -> f a
          asArgsType = const

instance Typeable1 f => Typeable1 (LoweredFunctor f) where
  typeOf1 tfa = mkTyConApp loweredTyCon [typeOf1 (undefined `asArgsType` tfa)]
    where asArgsType :: f a -> t f a -> f a
          asArgsType = const

instance Typeable1 f => Typeable1 (LiftedContravariant f) where
  typeOf1 tfa = mkTyConApp liftedContraTyCon [typeOf1 (undefined `asArgsType` tfa)]
    where asArgsType :: f a -> t f a -> f a
          asArgsType = const

instance Typeable1 f => Typeable1 (LoweredContravariant f) where
  typeOf1 tfa = mkTyConApp loweredContraTyCon [typeOf1 (undefined `asArgsType` tfa)]
    where asArgsType :: f a -> t f a -> f a
          asArgsType = const
#endif

liftedConstr, loweredConstr, liftedContraConstr, loweredContraConstr :: Constr
liftedConstr = mkConstr liftedDataType "LiftedFunctor" [] Prefix
loweredConstr = mkConstr loweredDataType "LoweredFunctor" [] Prefix
liftedContraConstr = mkConstr liftedContraDataType "LiftedContravariant" [] Prefix
loweredContraConstr = mkConstr loweredContraDataType "LoweredContravariant" [] Prefix
{-# NOINLINE liftedConstr #-}
{-# NOINLINE loweredConstr #-}
{-# NOINLINE liftedContraConstr #-}
{-# NOINLINE loweredContraConstr #-}


liftedDataType, loweredDataType, liftedContraDataType, loweredContraDataType :: DataType
liftedDataType = mkDataType "Control.Categorical.Fucntor.LiftedFunctor" [liftedConstr]
loweredDataType = mkDataType "Control.Categorical.Fucntor.LoweredFunctor" [loweredConstr]
liftedContraDataType = mkDataType "Control.Categorical.Fucntor.LiftedContravariant" [liftedContraConstr]
loweredContraDataType = mkDataType "Control.Categorical.Fucntor.LoweredContravariant" [loweredContraConstr]
{-# NOINLINE liftedDataType #-}
{-# NOINLINE loweredDataType #-}
{-# NOINLINE liftedContraDataType #-}
{-# NOINLINE loweredContraDataType #-}

instance (Typeable1 f, Data (f a), Data a) => Data (LiftedFunctor f a) where
  gfoldl f z (LiftedFunctor a) = z LiftedFunctor `f` a
  toConstr _ = liftedConstr
  gunfold k z c = case constrIndex c of
    1 -> k (z LiftedFunctor)
    _ -> error "gunfold"
  dataTypeOf _ = liftedDataType
  dataCast1 f = gcast1 f

instance (Typeable1 f, Data (f a), Data a) => Data (LoweredFunctor f a) where
  gfoldl f z (LoweredFunctor a) = z LoweredFunctor `f` a
  toConstr _ = loweredConstr
  gunfold k z c = case constrIndex c of
    1 -> k (z LoweredFunctor)
    _ -> error "gunfold"
  dataTypeOf _ = loweredDataType
  dataCast1 f = gcast1 f

instance (Typeable1 f, Data (f a), Data a) => Data (LiftedContravariant f a) where
  gfoldl f z (LiftedContravariant a) = z LiftedContravariant `f` a
  toConstr _ = liftedContraConstr
  gunfold k z c = case constrIndex c of
    1 -> k (z LiftedContravariant)
    _ -> error "gunfold"
  dataTypeOf _ = liftedContraDataType
  dataCast1 f = gcast1 f

instance (Typeable1 f, Data (f a), Data a) => Data (LoweredContravariant f a) where
  gfoldl f z (LoweredContravariant a) = z LoweredContravariant `f` a
  toConstr _ = loweredContraConstr
  gunfold k z c = case constrIndex c of
    1 -> k (z LoweredContravariant)
    _ -> error "gunfold"
  dataTypeOf _ = loweredContraDataType
  dataCast1 f = gcast1 f

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
