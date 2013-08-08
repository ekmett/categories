-------------------------------------------------------------------------------------------
-- |
-- Copyright : 2008-2013 Edward Kmett
-- License   : BSD
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Make it clearer when we are dealing with the category (->) that we mean the category
-- of haskell types via its Hom bifunctor (->)
-------------------------------------------------------------------------------------------
module Control.Category.Hask
  ( Hask
  ) where

type Hask = (->)
