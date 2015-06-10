{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Math.Functor.Faithful where

import Data.Constraint
import Math.Category
import Math.Functor
import Prelude (($))

class Functor f => FullyFaithful f where
  unfmap :: Cod f (f a) (f b) -> Dom f a b

instance FullyFaithful Dict where
  unfmap f = Sub $ f Dict

instance FullyFaithful (->) where
  unfmap (Nat f) = Op (f id)

instance FullyFaithful (:-) where
  unfmap (Nat f) = Op (f id)
