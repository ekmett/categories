{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
-------------------------------------------------------------------------------------------
-- |
-- Module   : Control.Category.Object
-- Copyright: 2010-2014 Edward Kmett
-- License  : BSD
--
-- Maintainer : Edward Kmett <ekmett@gmail.com>
-- Stability  : experimental
-- Portability: non-portable (either class-associated types or MPTCs with fundeps)
--
-- This module declares the 'HasTerminalObject' and 'HasInitialObject' classes.
--
-- These are both special cases of the idea of a (co)limit.
-------------------------------------------------------------------------------------------

module Control.Category.Object
  ( HasTerminalObject(..)
  , HasInitialObject(..)
  ) where

import Control.Category
import Data.Void

-- | The @Category (~>)@ has a terminal object @Terminal (~>)@ such that for all objects @a@ in @(~>)@,
-- there exists a unique morphism from @a@ to @Terminal (~>)@.
class Category k => HasTerminalObject (k :: x -> x -> *) where
  type Terminal k :: x
  terminate :: a `k` Terminal k

instance HasTerminalObject (->) where
  type Terminal (->) = ()
  terminate _ = ()

-- | The @Category (~>)@ has an initial (coterminal) object @Initial (~>)@ such that for all objects
-- @a@ in @(~>)@, there exists a unique morphism from @Initial (~>) @ to @a@.

class Category k => HasInitialObject (k :: x -> x -> *) where
  type Initial k :: x
  initiate :: Initial k `k` a

instance HasInitialObject (->) where
  type Initial (->) = Void
  initiate = absurd
