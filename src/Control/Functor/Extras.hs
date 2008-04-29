{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Functor.Extras
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
--
----------------------------------------------------------------------------
module Control.Functor.Extras where

type Dist f g = forall a. f (g a) -> g (f a)
type Natural f g = forall a. f a -> g a

class PostFold m f where
        postFold :: f (m (f a)) -> m (f a)

class PostUnfold w f where
        postUnfold :: w (f a) -> f (w (f a))

class PreFold f m where
        preFold :: f (m (f a)) -> f (m a)

class PreUnfold f w where
        preUnfold :: f (w a) -> f (w (f a))

class Distributes f g where
        dist :: f (g a) -> g (f a)
