{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
module Control.Categorical.Category
  ( Category(..)
  , Dual(..)
  ) where

import Prelude hiding (id, (.))

-- | Export our own definition until base is PolyKinded.
class Category (k :: x -> x -> *) where
  id :: k a a
  (.) :: k b c -> k a b -> k a c

instance Category (->) where
  id x = x
  {-# INLINE id #-}
  f . g = \x -> f (g x)
  {-# INLINE (.) #-}

newtype Dual (k :: x -> x -> *) (a :: x) (b :: x) = Dual { runDual :: k b a }
  deriving (Eq,Ord,Show,Read)

instance Category k => Category (Dual k) where
  id = Dual id
  {-# INLINE id #-}
  Dual f . Dual g = Dual (g . f)
  {-# INLINE (.) #-}
