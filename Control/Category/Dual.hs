{-# LANGUAGE TypeOperators, FlexibleContexts #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Dual
-- Copyright    : 2008-2010 Edward Kmett
-- License      : BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-------------------------------------------------------------------------------------------
module Control.Category.Dual
	( Dual(..)
	) where

import Prelude (undefined,const,error)
import Control.Category

#ifdef __GLASGOW_HASKELL__
import Data.Data (Data(..), mkDataType, DataType, mkConstr, Constr, constrIndex, Fixity(..))
import Data.Typeable (Typeable2(..), TyCon, mkTyCon, mkTyConApp, gcast1)
#endif

data Dual k a b = Dual { runDual :: k b a } 

instance Category k => Category (Dual k) where
	id = Dual id
	Dual f . Dual g = Dual (g . f)

#ifdef __GLASGOW_HASKELL__
instance Typeable2 (~>) => Typeable2 (Dual (~>)) where
    typeOf2 tfab = mkTyConApp dataTyCon [typeOf2 (undefined `asDualArgsType` tfab)]
        where asDualArgsType :: f b a -> t f a b -> f b a
              asDualArgsType = const

dataTyCon :: TyCon
dataTyCon = mkTyCon "Control.Category.Dual.Dual"
{-# NOINLINE dataTyCon #-}

dualConstr :: Constr
dualConstr = mkConstr dataDataType "Dual" [] Prefix
{-# NOINLINE dualConstr #-}

dataDataType :: DataType
dataDataType = mkDataType "Control.Category.Dual.Dual" [dualConstr]
{-# NOINLINE dataDataType #-}

instance (Typeable2 (~>), Data a, Data b, Data (b ~> a)) => Data (Dual (~>) a b) where
    gfoldl f z (Dual a) = z Dual `f` a
    toConstr _ = dualConstr
    gunfold k z c = case constrIndex c of
        1 -> k (z Dual)
        _ -> error "gunfold"
    dataTypeOf _ = dataDataType
    dataCast1 f = gcast1 f
#endif
