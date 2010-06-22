-------------------------------------------------------------------------------------------
-- |
-- Module    : Control.Category.Distributive
-- Copyright     : 2008 Edward Kmett
-- License    : BSD
--
-- Maintainer    : Edward Kmett <ekmett@gmail.com>
-- Stability    : experimental
-- Portability    : non-portable (class-associated types)
--
-------------------------------------------------------------------------------------------
module Control.Category.Distributive
    ( 
    -- * Distributive Categories
      factor
    , Distributive(..)
    ) where

import Prelude hiding (Functor, map, (.), id, fst, snd, curry, uncurry)
import Categorical.Bifunctor
import Control.Category
import Control.Category.Cartesian

-- | the canonical factoring morphism 
factor :: ( PreCartesian (~>)
          , (*) ~ Product (~>)
          , PreCoCartesian (~>)
          , (+) ~ Sum (~>) 
          ) => ((a * b) + (a * c)) ~> (a * (b + c))
factor = second inl ||| second inr

-- | A category in which 'factor' is an isomorphism
class ( PreCartesian (~>) 
      , (*) ~ Product (~>)
      , PreCoCartesian (~>)
      , (+) ~ Sum (~>) 
      ) => Distributive (~>) where
    distribute :: (a * (b + c)) ~> ((a * b) + (a * c))

instance Distributive (->) where
    distribute (a, Left b) = Left (a,b)
    distribute (a, Right c) = Right (a,c)

{-# RULES
"factor . distribute" factor . distribute = id
"distribute . factor" distribute . factor = id
 #-}
