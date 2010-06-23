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
-- 
-- > factor :: ( PreCartesian k
-- >         , (*) ~ Product k
-- >         , PreCoCartesian k
-- >         , (+) ~ Sum k 
-- >         ) => ((a * b) + (a * c)) `k` (a * (b + c))

factor :: ( PreCartesian k
          , PreCoCartesian k
          ) => Sum k (Product k a b) (Product k a c) `k` Product k a (Sum k b c)
factor = second inl ||| second inr

-- | A category in which 'factor' is an isomorphism
--
-- > class ( PreCartesian k 
-- >       , (*) ~ Product k
-- >       , PreCoCartesian k
-- >       , (+) ~ Sum k 
-- >       ) => Distributive k where

class (PreCartesian k, PreCoCartesian k) => Distributive k where
    distribute :: Product k a (Sum k b c) `k` Sum k (Product k a b) (Product k a c)

instance Distributive (->) where
    distribute (a, Left b) = Left (a,b)
    distribute (a, Right c) = Right (a,c)

{-# RULES
"factor . distribute" factor . distribute = id
"distribute . factor" distribute . factor = id
  #-}
