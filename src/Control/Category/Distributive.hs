{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE TypeOperators #-}
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
module Control.Category.Distributive
    (
    -- * Distributive Categories
      factor
    , Distributive(..)
    ) where

import Prelude hiding (Functor, map, (.), id, fst, snd, curry, uncurry)
import Control.Categorical.Bifunctor
import Control.Category.Monoidal
import Control.Category.Cartesian

-- | The canonical factoring morphism.
factor :: (Cartesian k, CoCartesian k) => Sum k (Product k a b) (Product k a c) `k` Product k a (Sum k b c)
factor = second inl ||| second inr

-- | A category in which 'factor' is an natural isomorphism
class (Cartesian k, CoCartesian k) => Distributive (k :: x -> x -> *) where
    -- |
    --
    -- @
    -- factor . distribute = id
    -- distribute . factor = id
    -- @
    distribute :: Product k a (Sum k b c) `k` Sum k (Product k a b) (Product k a c)

instance Distributive (->) where
    distribute (a, Left b) = Left (a,b)
    distribute (a, Right c) = Right (a,c)
