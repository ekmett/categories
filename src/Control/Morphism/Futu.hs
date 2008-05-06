{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Morphism.Futu
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
-- 
-- Traditional operators, shown here to show how to roll your own
----------------------------------------------------------------------------
module Control.Morphism.Futu where

import Control.Functor.Algebra
import Control.Functor.Extras
import Control.Functor.Fix
import Control.Comonad ()
import Control.Monad.Free
import Control.Morphism.Ana

futu :: Functor f => CoAlgM f (Free f) a -> a -> Fix f
futu = g_ana (distFutu id)

g_futu :: (Functor f, Functor h) => Dist h f -> CoAlgM f (Free h) a -> a -> Fix f
g_futu k = g_ana (distFutu k)

distFutu :: (Functor f, Functor h) => Dist h f -> Dist (Free h) f
distFutu k = cataFree (fmap return) (fmap inFree . k)

