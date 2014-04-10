{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
-------------------------------------------------------------------------------------------
-- |
-- Copyright   : 2008-2014 Edward Kmett
-- License     : BSD3
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (functional-dependencies)
-------------------------------------------------------------------------------------------
module Control.Category.Dual
  ( Dual(..)
  ) where

import Control.Category
import Data.Typeable
import Prelude hiding ((.), id)

newtype Dual (k :: x -> x -> *) (a :: x) (b :: x) = Dual { runDual :: k b a }
  deriving (Eq,Ord,Show,Read,Typeable)


instance Category k => Category (Dual k) where
  id = Dual id
  {-# INLINE id #-}
  Dual f . Dual g = Dual (g . f)
  {-# INLINE (.) #-}
