-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Indexed.Fix
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Monad.Indexed.Fix
	( IxMonadFix(..)
	) where

import Control.Monad.Indexed

class IxMonad m => IxMonadFix m where
	imfix :: (a -> m i i a) -> m i i a

