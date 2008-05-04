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

import Control.Bifunctor
import Control.Bifunctor.HigherOrder
import Control.Comonad
import Control.Monad
import Control.Functor.Algebra
import Control.Functor.Extras
import Control.Functor.Fix
import Control.Functor.HigherOrder

hylo :: Functor f => Alg g b -> Natural f g -> CoAlg f a -> a -> b
hylo f e g = f . e . fmap (hylo f e g). g 

g_hylo :: (Comonad w, Functor f, Monad m) =>
          Dist g w -> Dist m f -> AlgW g w b -> Natural f g -> CoAlgM f m a -> a -> b
g_hylo w m f e g = extract . h . return where h = liftW f . w . e . fmap (duplicate . h . join) . m . liftM g


-- A more "Jeremy Gibbons"-style bifunctor-based version has the same expressive power 

bihylo :: (Bifunctor f, Bifunctor g) => Alg (g d) b -> Natural (f c) (g d) -> CoAlg (f c) a -> a -> b
bihylo f e g = f . e . bimap id (bihylo f e g). g 

g_bihylo :: (Comonad w, Bifunctor f, Monad m) =>
          Dist (g d) w -> Dist m (f c) -> AlgW (g d) w b -> Natural (f c) (g d) -> CoAlgM (f c) m a -> a -> b
g_bihylo w m f e g = extract . h . return where h = liftW f . w . e . bimap id (duplicate . h . join) . m . liftM g


-- | higher order hylomorphisms for use in building up and tearing down higher order functors
hhylo :: HFunctor f => AlgH f b -> CoAlgH f a -> Natural a b
hhylo f g = f . hfmap (hhylo f g) . g

