-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Indexed
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable 
--
----------------------------------------------------------------------------
module Control.Comonad.Indexed where

import Control.Comonad
import Control.Arrow
import Control.Functor.Extras
import Control.Functor.HigherOrder
import Control.Functor.Indexed
import Control.Monad

class IxFunctor w => IxComonad w where
	iextract :: w i i a -> a
	iextend :: (w j k a -> b) -> w i k a -> w i j b

iduplicate :: IxComonad w => w i k a -> w i j (w j k a)
iduplicate = iextend id

instance Comonad w => IxComonad (LiftIx w) where
	iextract = extract . lowerIx 
	iextend f = LiftIx . extend (f . LiftIx) . lowerIx
