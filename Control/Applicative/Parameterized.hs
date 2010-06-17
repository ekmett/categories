{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Applicative.Paramterized
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Applicative.Parameterized 
	( PApplicative(..)
	, PPointed(..)
	) where

import Control.Functor.Pointed

class PPointed f => PApplicative f where
	pap :: f (a -> b) c -> f a c -> f b c
