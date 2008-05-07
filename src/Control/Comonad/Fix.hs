-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Comonad.Fix
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-------------------------------------------------------------------------------------------
module Control.Comonad.Fix 
	( ComonadFix(..)
	) where

import Control.Comonad
import Control.Monad.Identity

class Comonad w => ComonadFix w where
	cofix :: w (w a -> a) -> a

instance ComonadFix Identity where
	cofix (Identity f) = fix (f . Identity)

instance ComonadFix ((,)e) where
	cofix ~(e,f) = let x = f (e,x) in x
