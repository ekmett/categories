-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.HigherOrder
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
--
-- extending Neil Ghani and Patrician Johann's HFunctor to higher order comonads
----------------------------------------------------------------------------
module Control.Comonad.HigherOrder where

import Control.Functor.Extras
import Control.Functor.HigherOrder

class HFunctor w => HComonad w where
	hextract :: Functor f => Natural (w f) f
	hextend  :: (Functor f, Functor g) => Natural (w f) g -> Natural (w f) (w g)
