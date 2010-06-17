{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Functor.Combinators.Join
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Functor.Combinators.Join
	( Join(..)
	) where

import Control.Functor
import Control.Category.Hask

newtype Join p a = Join { runJoin :: p a a } 

instance Bifunctor p Hask Hask Hask => Functor (Join p) where
	fmap f = Join . bimap f f . runJoin
