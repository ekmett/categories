{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Morphism.Build
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
-- 
----------------------------------------------------------------------------
module Control.Morphism.Build where

import Control.Functor.Extras
import Control.Functor.HigherOrder
import Control.Functor.KanExtension
-- import Control.Functor.KanExtension.Interpreter
-- import Control.Morphism.Cata

-- | @forall h g.  hcata h . hbuild g = g h@ cannot be realized as a RULE because
-- h and g are not monotypes.
-- Kan extended build, gbuild in Ghani/Johann parlance, but g_foo currently denotes
-- generalized in the 'has a parameterizing (co)monad' sense.
hbuild :: (HFunctor f, Functor c) => (forall x. HAlgebra f x -> c :~> x) -> c :~> FixH f
hbuild g = g InH

-- | @ forall h g. kcata h . kbuild g = g (interpreterAlgebra h)@ cannot be realized as 
-- a RULE because h and g are not monotypes.
kbuild :: HFunctor f => (forall x. HAlgebra f x -> Lan g h :~> x) -> Lan g h :~> FixH f
kbuild = hbuild
