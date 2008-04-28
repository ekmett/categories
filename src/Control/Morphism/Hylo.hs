{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Morphism.Hylo
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
--
-- Generalized hylomorphisms 
----------------------------------------------------------------------------
module Control.Morphism.Hylo where

import Control.Comonad
import Control.Monad
import Control.Functor.Algebra
import Control.Functor.Extras
import Control.Functor.Fix

hylo :: (Functor f, Functor g) => Alg g b -> Natural f g -> CoAlg f a -> a -> b
hylo f e g = f . e . fmap (hylo f e g). g 

g_hylo :: (Comonad w, Functor f, Monad m) =>
          Dist g w -> Dist m f -> AlgW g w b -> Natural f g -> CoAlgM f m a -> a -> b
g_hylo w m f e g = extract . h . return where h = liftW f . w . e . fmap (duplicate . h . join) . m . liftM g

