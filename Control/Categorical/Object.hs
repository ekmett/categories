{-# LANGUAGE TypeFamilies, TypeOperators #-}
-------------------------------------------------------------------------------------------
-- |
-- Module   : Control.Category.Object
-- Copyright: 2010 Edward Kmett
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

module Control.Categorical.Object 
    ( HasTerminalObject(..)
    , HasInitialObject(..)
    ) where

import Control.Category

-- | The @Category (~>)@ has a terminal object @Terminal (~>)@ such that for all objects @a@ in @(~>)@, 
-- there exists a unique morphism from @a@ to @Terminal (~>)@.
class Category (~>) => HasTerminalObject (~>) where
    type Terminal (~>) :: *
    terminate :: a ~> Terminal (~>)

-- | The @Category (~>)@ has an initial (coterminal) object @Initial (~>)@ such that for all objects 
-- @a@ in @(~>)@, there exists a unique morphism from @Initial (~>) @ to @a@.

class Category (~>) => HasInitialObject (~>) where
    type Initial (~>) :: *
    initiate :: Initial (~>) ~> a
