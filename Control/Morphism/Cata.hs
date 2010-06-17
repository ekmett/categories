{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Morphism.Cata
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
-- 
----------------------------------------------------------------------------
module Control.Morphism.Cata 
	( cata, g_cata, distCata
	, bicata, g_bicata
	, hcata
	, kcata, runkcata
	) where

import Control.Comonad
import Control.Category.Hask
import Control.Functor
import Control.Functor.Pointed
import Control.Functor.Algebra 
import Control.Functor.Extras
import Control.Functor.Fix
import Control.Functor.HigherOrder
import Control.Functor.KanExtension
import Control.Functor.KanExtension.Interpreter
import Control.Monad.Identity

cata :: Functor f => Algebra f a -> FixF f -> a
cata f = f . fmap (cata f) . outF
-- cata f = g_cata distCata (liftAlgebra f)

g_cata :: (Functor f, Comonad w) => Dist f w -> GAlgebra f w a -> FixF f -> a
g_cata k g = extract . c where c = liftW g . k . fmap (duplicate . c) . outF
-- g_cata k f = g_hylo k distAna f id outM

distCata :: Functor f => Dist f Identity
distCata = Identity . fmap runIdentity

bicata :: QFunctor f Hask Hask => Algebra (f b) a -> Fix f b -> a
bicata f = f . second (bicata f) . outB

g_bicata :: (QFunctor f Hask Hask, Comonad w) => Dist (f b) w -> GAlgebra (f b) w a -> Fix f b -> a
g_bicata k g = extract . c where c = liftW g . k . second (duplicate . c) . outB

hcata :: HFunctor f => HAlgebra f a -> FixH f :~> a
hcata f = f . hfmap (hcata f) . outH

kcata :: HFunctor f => InterpreterT f g h -> FixH f :~> Ran g h
kcata i = hcata (interpreterAlgebra i)

runkcata :: HFunctor f => InterpreterT f g h -> FixH f a -> (a -> g b) -> h b
runkcata i = runRan . kcata i
