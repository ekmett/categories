{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Morphism.Exo
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
--
-- Martin Erwig's exomorphism
----------------------------------------------------------------------------
module Control.Morphism.Exo 
	( exo
	) where

import Control.Functor.Algebra
import Control.Morphism.Hylo

-- | Martin Erwig's exomorphism from d to d'
exo :: Functor h => Bialgebra m n b -> (h b -> m b) -> (h a -> h (g a)) -> Trialgebra f g h a -> g a -> b
exo d' f g d = hylo (fst d' . f) id (g . snd d)

