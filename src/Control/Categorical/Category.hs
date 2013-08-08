{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}

module Control.Categorical.Category
  ( Category(..) ) where

import Prelude hiding (id, (.))

-- | Export our own definition until base is PolyKinded.
class Category (k :: x -> x -> *) where
  id :: k a a
  (.) :: k b c -> k a b -> k a c

instance Category (->) where
  id x = x
  f . g = \x -> f (g x)


