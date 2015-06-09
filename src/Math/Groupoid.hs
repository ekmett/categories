{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Math.Groupoid where

import Math.Category

class Category p => Groupoid p where
  inv :: p a b -> p b a

instance (Groupoid p, Op p ~ Yoneda p) => Groupoid (Yoneda p) where
  inv (Op f) = Op (inv f)
