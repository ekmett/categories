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
        hunit   :: Natural a (g (f a))
        hcounit :: Natural (f (g b)) b
        hleftAdjunct  :: Natural (f a) b -> Natural a (g b)
        hrightAdjunct :: Natural a (g b) -> Natural (f a) b

        hunit = hleftAdjunct id
        hcounit = hrightAdjunct id
        hleftAdjunct f = hfmap f . hunit
        hrightAdjunct f = hcounit . hfmap f
