-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Functor.Algebra
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
--
----------------------------------------------------------------------------
module Control.Functor.Algebra where

import Control.Comonad

type Alg f a = f a -> a
type CoAlg f a = a -> f a
type AlgW f w a = f (w a) -> a
type CoAlgM f m a = a -> f (m a)

liftAlg :: (Functor f, Comonad w) => Alg f a -> AlgW f w a
liftAlg f = f . fmap extract

liftCoAlg :: (Functor f, Monad m) => CoAlg f a -> CoAlgM f m a
liftCoAlg f = fmap return . f

