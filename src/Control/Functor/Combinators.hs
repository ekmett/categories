{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Combinators
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-- Generalized functor combinators. The names are indicative of term-level combinators
-- where possible to aid intuition
-------------------------------------------------------------------------------------------

module Control.Functor.Combinators
	( module Control.Functor.Combinators.Biff
	, module Control.Functor.Combinators.Const
	, module Control.Functor.Combinators.Join
	, module Control.Functor.Combinators.Lift
	, module Control.Functor.Combinators.Of
	) where

import Control.Functor.Combinators.Biff
import Control.Functor.Combinators.Const
import Control.Functor.Combinators.Join
import Control.Functor.Combinators.Lift
import Control.Functor.Combinators.Of
