{-# OPTIONS_GHC -fglasgow-exts #-}
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

import Control.Functor
import Control.Category
import Control.Category.Hask
import Prelude hiding ((.),id)
import Control.Comonad
import Control.Monad
import Control.Functor.Algebra
import Control.Functor.Extras
import Control.Functor.HigherOrder

-- | hylo :: (g b -> b) -> (forall c. f c -> g c) -> (a -> f b) -> a -> b
hylo :: Functor f => Algebra g b -> (f :~> g) -> Coalgebra f a -> a -> b
hylo f e g = f . e . fmap (hylo f e g). g 

-- | g_hylo :: (Comonad w, Functor f, Monad m) => (forall d. g (w d) -> w (g d)) -> (forall e. m (f e) -> f (m e)) -> (g (w b) -> b) -> (forall c. f c -> g c) -> a -> f (m a) -> a -> b
g_hylo :: (Comonad w, Functor f, Monad m) => Dist g w -> Dist m f -> GAlgebra g w b -> (f :~> g) -> GCoalgebra f m a -> a -> b
g_hylo w m f e g = extract . h . return where h = liftW f . w . e . fmap (duplicate . h . join) . m . liftM g

-- The Jeremy Gibbons-style bifunctor-based version has the same expressive power, but may fuse with bimaps better

bihylo :: (QFunctor f Hask Hask) => Algebra (g d) b -> (f c :~> g d) -> Coalgebra (f c) a -> a -> b
bihylo f e g = f . e . second (bihylo f e g). g 

g_bihylo :: (Comonad w, QFunctor f Hask Hask, Monad m) =>
          Dist (g d) w -> Dist m (f c) -> GAlgebra (g d) w b -> (f c :~> g d) -> GCoalgebra (f c) m a -> a -> b
g_bihylo w m f e g = extract . h . return where h = liftW f . w . e . second (duplicate . h . join) . m . liftM g

-- | higher order hylomorphisms for use in building up and tearing down higher order functors
hhylo :: HFunctor f => HAlgebra f b -> HCoalgebra f a -> a :~> b
hhylo f g = f . hfmap (hhylo f g) . g

