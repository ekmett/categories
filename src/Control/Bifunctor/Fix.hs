-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Bifunctor.Fix
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-------------------------------------------------------------------------------------------
module Control.Bifunctor.Fix where

import Control.Bifunctor

newtype FixB s a = InB { outB :: s a (FixB s a) }

instance Bifunctor s => Functor (FixB s) where
        fmap f = InB . bimap f (fmap f) . outB
