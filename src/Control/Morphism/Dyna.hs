{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Morphism.Dyna
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
-- 
----------------------------------------------------------------------------
module Control.Morphism.Dyna where

import Control.Functor.Algebra
import Control.Functor.Extras
import Control.Comonad.Cofree
import Control.Morphism.Hylo
import Control.Morphism.Histo
import Control.Morphism.Ana

dyna :: (Functor f, Functor g) => AlgW g (Cofree g) b -> (f :~> g) -> CoAlg f a -> a -> b
dyna f e g = g_hylo (distHisto id) distAna f e (liftCoAlg g)
