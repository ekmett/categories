{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Morphism.Destroy
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
-- 
----------------------------------------------------------------------------
module Control.Morphism.Destroy where

import Control.Functor.Extras
import Control.Functor.HigherOrder
import Control.Functor.KanExtension
-- import Control.Morphism.Ana

-- | @forall h g . hdestroy g . hana h = g h@ cannot be realized as a RULE.
hdestroy :: (HFunctor f, Functor c) => (forall g. HCoalgebra f g -> g :~> c) -> FixH f :~> c
hdestroy g = g outH

-- | @forall h g . kdestroy g . kana h = g (cointerpreterCoalgebra h)@ cannot be realized as a RULE
kdestroy :: HFunctor f => (forall x. HCoalgebra f x -> x :~> Ran g h) -> FixH f :~> Ran g h
kdestroy = kdestroy
