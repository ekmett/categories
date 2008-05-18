-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Functor.Adjunction.HigherOrder
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
--
-- Higher-Order Adjunctions
----------------------------------------------------------------------------
module Control.Functor.Adjunction.HigherOrder 
	( HAdjunction(..)
	) where

import Control.Functor.HigherOrder
import Control.Functor.Extras

class (HFunctor f, HFunctor g) => HAdjunction f g where
        hunit   :: a :~> g (f a)
        hcounit :: f (g b) :~> b
        hleftAdjunct  :: (f a :~> b) -> a :~> g b
        hrightAdjunct :: (a :~> g b) -> f a :~> b

        hunit = hleftAdjunct id
        hcounit = hrightAdjunct id
        hleftAdjunct f = hfmap f . hunit
        hrightAdjunct f = hcounit . hfmap f
