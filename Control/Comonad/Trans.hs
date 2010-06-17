{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Trans
-- Copyright   :  (C) 2008 Edward Kmett
--		  (C) 2004 Dave Menendez
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
----------------------------------------------------------------------------
module Control.Comonad.Trans
	( ComonadTrans(colift)
	) where

import Control.Comonad

class ComonadTrans t where
	colift :: Comonad w => w a -> t w a 
