{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Context.Class
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Comonad.Context.Class where

import Control.Comonad

class Comonad w => ComonadContext s w | w -> s where
	getC :: w a -> s
	modifyC :: (s -> s) -> w a -> a 

putC :: ComonadContext s w => s -> w a -> a
putC = modifyC . const 

experiment :: (ComonadContext s w, Functor f) => f (s -> s) -> w a -> f a
experiment ms a = fmap (flip modifyC a) ms
