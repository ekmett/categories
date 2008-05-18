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
module Control.Comonad.HigherOrder 
	( HFunctor(..)
	, HCopointed(..)
	, HComonad(..)
	, hduplicate
	) where

import Control.Functor.Extras
import Control.Functor.HigherOrder

class HCopointed w => HComonad w where
	hextend  :: (Functor f, Functor g) => (w f :~> g) -> w f :~> w g


hduplicate :: (HComonad w, Functor (w g), Functor g) => w g :~> w (w g)
hduplicate = hextend id
