{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators, FlexibleContexts #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE DeriveDataTypeable #-}
#endif
-------------------------------------------------------------------------------------------
-- |
-- Module   : Control.Category.Dual
-- Copyright: 2008-2010 Edward Kmett
-- License  : BSD
--
-- Maintainer : Edward Kmett <ekmett@gmail.com>
-- Stability  : experimental
-- Portability: portable
--
-------------------------------------------------------------------------------------------
module Control.Category.Dual
  ( Dual(..)
  ) where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

import Control.Category

#ifdef __GLASGOW_HASKELL__
import Data.Data (Data(..), mkDataType, DataType, mkConstr, Constr, constrIndex, Fixity(..))
#if __GLASGOW_HASKELL__ < 708
#if MIN_VERSION_base(4,4,0)
import Data.Typeable (Typeable2(..), TyCon, mkTyCon3, mkTyConApp, gcast1)
#else
import Data.Typeable (Typeable2(..), TyCon, mkTyCon, mkTyConApp, gcast1)
#endif
import Prelude (undefined,const,error)
#else
import Prelude (error)
import Data.Typeable (Typeable, gcast1)
#endif
#endif

newtype Dual k a b = Dual { runDual :: k b a }
#if __GLASGOW_HASKELL__ >= 708
  deriving Typeable

#define Typeable2 Typeable
#endif

instance Category k => Category (Dual k) where
  id = Dual id
  Dual f . Dual g = Dual (g . f)

#ifdef __GLASGOW_HASKELL__

#if __GLASGOW_HASKELL__ < 707
instance Typeable2 k => Typeable2 (Dual k) where
  typeOf2 tfab = mkTyConApp dataTyCon [typeOf2 (undefined `asDualArgsType` tfab)]
    where asDualArgsType :: f b a -> t f a b -> f b a
          asDualArgsType = const

dataTyCon :: TyCon
#if MIN_VERSION_base(4,4,0)
dataTyCon = mkTyCon3 "categories" "Control.Category.Dual" "Dual"
#else
dataTyCon = mkTyCon "Control.Category.Dual.Dual"
#endif
{-# NOINLINE dataTyCon #-}
#endif

dualConstr :: Constr
dualConstr = mkConstr dataDataType "Dual" [] Prefix
{-# NOINLINE dualConstr #-}

dataDataType :: DataType
dataDataType = mkDataType "Control.Category.Dual.Dual" [dualConstr]
{-# NOINLINE dataDataType #-}

instance (Typeable2 k, Data a, Data b, Data (k b a)) => Data (Dual k a b) where
  gfoldl f z (Dual a) = z Dual `f` a
  toConstr _ = dualConstr
  gunfold k z c = case constrIndex c of
    1 -> k (z Dual)
    _ -> error "gunfold"
  dataTypeOf _ = dataDataType
  dataCast1 f = gcast1 f
#endif
