{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Comonad where

import Control.Monad
import Control.Arrow ((|||), (&&&), (+++), (***))

class Functor w => Comonad w where
        duplicate :: w a -> w (w a)
        extend :: (w a -> b) -> w a -> w b
        extract :: w a -> a
        extend f = fmap f . duplicate
        duplicate = extend id

liftW :: Comonad w => (a -> b) -> w a -> w b
liftW f = extend (f . extract)

liftCtx :: Comonad w => (a -> b) -> w a -> b
liftCtx f = extract . fmap f
