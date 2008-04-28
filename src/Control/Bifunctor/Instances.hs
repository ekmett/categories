{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Bifunctor.Instances
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Bifunctor.Instances where

import Control.Bifunctor
import Control.Arrow ((***), (+++))

instance Bifunctor (,) where
        bimap = (***)

instance Bifunctor Either where
	bimap = (+++)
