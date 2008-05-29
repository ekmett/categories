{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Stream
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Comonad.Stream
	( Stream
	) where


import Control.Comonad.Cofree
import Control.Monad.Identity
type Stream = Cofree Identity

-- class ComonadStream w where fby :: a -> (w a -> a) 
-- next :: w a -> w a 
-- run :: (ComonadStream w, ComonadContext Int c) => (c a -> b) -> w a -> w b
