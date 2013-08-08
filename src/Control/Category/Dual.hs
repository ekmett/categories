{-# LANGUAGE TypeOperators, FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-------------------------------------------------------------------------------------------
-- |
-- Copyright: 2008-2013 Edward Kmett
-- License  : BSD
--
-- Maintainer : Edward Kmett <ekmett@gmail.com>
-- Stability  : experimental
-- Portability: non-portable
--
-------------------------------------------------------------------------------------------
module Control.Category.Dual
  ( Dual(..)
  ) where

import Prelude (undefined,const,error)
import Control.Categorical.Category

newtype Dual (k :: x -> x -> *) (a :: x) (b :: x) = Dual { runDual :: k b a }

instance Category k => Category (Dual k) where
  id = Dual id
  Dual f . Dual g = Dual (g . f)
