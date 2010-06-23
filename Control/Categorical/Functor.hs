{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DeriveDataTypeable, FlexibleContexts, UndecidableInstances, FlexibleInstances #-}
-------------------------------------------------------------------------------------------
-- |
-- Module     : Control.Categorical.Functor
-- Copyright : 2008-2010 Edward Kmett
-- License     : BSD3
--
-- Maintainer   : Edward Kmett <ekmett@gmail.com>
-- Stability    : experimental
-- Portability    : non-portable (functional-dependencies)
--
-- A more categorical definition of 'Functor'
-------------------------------------------------------------------------------------------
module Control.Categorical.Functor 
    ( Functor(fmap) 
    , EndoFunctor
    , LiftedFunctor(..)
    , LoweredFunctor(..)
    ) where

import Control.Category
import Prelude hiding (id, (.), Functor(..))
import qualified Prelude
import Data.Data (Data(..), mkDataType, DataType, mkConstr, Constr, constrIndex, Fixity(..))
import Data.Typeable (Typeable1(..), TyCon, mkTyCon, mkTyConApp, gcast1)

-- TODO Data, Typeable
newtype LiftedFunctor f a = LiftedFunctor (f a)
    deriving (Show, Read)

liftedTyCon :: TyCon
liftedTyCon = mkTyCon "Control.Categorical.Functor.LiftedFunctor"
{-# NOINLINE liftedTyCon #-}

liftedConstr :: Constr
liftedConstr = mkConstr liftedDataType "LiftedFunctor" [] Prefix
{-# NOINLINE liftedConstr #-}

liftedDataType :: DataType
liftedDataType = mkDataType "Control.Categorical.Fucntor.LiftedFunctor" [liftedConstr]
{-# NOINLINE liftedDataType #-}

instance Typeable1 f => Typeable1 (LiftedFunctor f) where
    typeOf1 tfa = mkTyConApp liftedTyCon [typeOf1 (undefined `asArgsType` tfa)]
        where asArgsType :: f a -> t f a -> f a
              asArgsType = const

instance (Typeable1 f, Data (f a), Data a) => Data (LiftedFunctor f a) where
    gfoldl f z (LiftedFunctor a) = z LiftedFunctor `f` a
    toConstr _ = liftedConstr
    gunfold k z c = case constrIndex c of
        1 -> k (z LiftedFunctor)
        _ -> error "gunfold"
    dataTypeOf _ = liftedDataType
    dataCast1 f = gcast1 f

newtype LoweredFunctor f a = LoweredFunctor (f a)
    deriving (Show, Read)

loweredTyCon :: TyCon
loweredTyCon = mkTyCon "Control.Categorical.Functor.LoweredFunctor"
{-# NOINLINE loweredTyCon #-}

loweredConstr :: Constr
loweredConstr = mkConstr loweredDataType "LoweredFunctor" [] Prefix
{-# NOINLINE loweredConstr #-}

loweredDataType :: DataType
loweredDataType = mkDataType "Control.Categorical.Fucntor.LoweredFunctor" [loweredConstr]
{-# NOINLINE loweredDataType #-}

instance Typeable1 f => Typeable1 (LoweredFunctor f) where
    typeOf1 tfa = mkTyConApp loweredTyCon [typeOf1 (undefined `asArgsType` tfa)]
        where asArgsType :: f a -> t f a -> f a
              asArgsType = const

instance (Typeable1 f, Data (f a), Data a) => Data (LoweredFunctor f a) where
    gfoldl f z (LoweredFunctor a) = z LoweredFunctor `f` a
    toConstr _ = loweredConstr
    gunfold k z c = case constrIndex c of
        1 -> k (z LoweredFunctor)
        _ -> error "gunfold"
    dataTypeOf _ = loweredDataType
    dataCast1 f = gcast1 f

class (Category r, Category t) => Functor f r t | f r -> t, f t -> r where
    fmap :: r a b -> t (f a) (f b)

instance Functor f (->) (->) => Prelude.Functor (LoweredFunctor f) where
    fmap f (LoweredFunctor a) = LoweredFunctor (Control.Categorical.Functor.fmap f a)

instance Prelude.Functor f => Functor (LiftedFunctor f) (->) (->) where
    fmap f (LiftedFunctor a) = LiftedFunctor (Prelude.fmap f a)

instance Functor ((,) a) (->) (->) where
    fmap f ~(a, b) = (a, f b)

instance Functor (Either a) (->) (->) where
    fmap _ (Left a) = Left a 
    fmap f (Right a) = Right (f a)

instance Functor Maybe (->) (->) where
    fmap = Prelude.fmap

instance Functor [] (->) (->) where
    fmap = Prelude.fmap

instance Functor IO (->) (->) where
    fmap = Prelude.fmap

class (Functor f (~>) (~>)) => EndoFunctor f (~>)
instance (Functor f (~>) (~>)) => EndoFunctor f (~>)
    
