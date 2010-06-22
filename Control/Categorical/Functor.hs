{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	 : Control.Category.Functor
-- Copyright : 2008-2010 Edward Kmett
-- License	 : BSD3
--
-- Maintainer   : Edward Kmett <ekmett@gmail.com>
-- Stability    : experimental
-- Portability	: non-portable (functional-dependencies)
--
-- A more categorical definition of 'Functor'
-------------------------------------------------------------------------------------------
module Control.Category.Bifunctor ( Functor(fmap) ) where

import Control.Category
import Prelude hiding (id, (.), Functor(..))
import qualified Prelude    

class (Category r, Category t) => Functor f r t | f r -> t, f t -> r where
	fmap :: r a b -> t (f a) (f b)

instance Functor ((,) a) (->) (->) where
	fmap f ~(a, b) = (a, f b)
    {-# INLINE fmap #-}

instance Functor (Either a) (->) (->) where
	fmap = Prelude.fmap
    {-# INLINE fmap #-}

instance Functor Maybe (->) (->) where
    fmap = Prelude.fmap
    {-# INLINE fmap #-}

instance Functor [] (->) (->) where
    fmap = Prelude.fmap
    {-# INLINE fmap #-}

instance Functor IO (->) (->) where
    fmap = Prelude.fmap
    {-# INLINE fmap #-}
