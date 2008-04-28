{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.State.Class
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Comonad.State.Class where

import Control.Comonad

class Comonad w => ComonadState s w | w -> s where
	getC :: w a -> w s
	putC :: s -> w a -> w a

-- modifyC :: (s -> s) -> StateC s a -> StateC s a
-- modifyC m (StateC f s) = StateC f (m s)
