{-# LANGUAGE PolyKinds #-}
module Math.Groupoid where

import Math.Category

class Category p => Groupoid p where
  inv :: p a b -> p b a
