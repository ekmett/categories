{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE TypeOperators #-}
-------------------------------------------------------------------------------------------
-- |
-- Module   : Control.Category.Distributive
-- Copyright: 2008 Edward Kmett
-- License  : BSD
--
-- Maintainer : Edward Kmett <ekmett@gmail.com>
-- Stability  : experimental
-- Portability: non-portable (class-associated types)
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
import Control.Category
import Control.Category.Cartesian

-- | The canonical factoring morphism.

factor :: (Cartesian k, CoCartesian k) => Sum k (Product k a b) (Product k a c) `k` Product k a (Sum k b c)
factor = second inl ||| second inr

-- | A category in which 'factor' is an isomorphism

class (Cartesian k, CoCartesian k) => Distributive k where
    distribute :: Product k a (Sum k b c) `k` Sum k (Product k a b) (Product k a c)

instance Distributive (->) where
    distribute (a, Left b) = Left (a,b)
    distribute (a, Right c) = Right (a,c)

{-- RULES
"factor . distribute" factor . distribute = id
"distribute . factor" distribute . factor = id
  --}
