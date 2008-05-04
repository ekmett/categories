-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Bifunctor.HigherOrder
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
--
----------------------------------------------------------------------------
module Control.Bifunctor.HigherOrder where

import Control.Bifunctor
import Control.Functor.Extras

{-
type BiNatural p q = forall a b. p a b -> q a b

class HBifunctor p where
        fbimap :: Bifunctor q => (a -> b) -> (c -> d) -> p q a c -> p q b d
	hbimap :: BiNatural g h -> BiNatural (p g) (p h)

newtype MuHB p a b = InHB { outHB :: p (MuHB p) a b }
type NuHB p a b = MuHB p a b
-}
