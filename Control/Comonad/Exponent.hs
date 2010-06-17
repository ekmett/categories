-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Exponent
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Comonad.Exponent
	( Exp(..)
	) where

import Data.Monoid
import Control.Comonad

data Exp m a = Exp { runExp :: m -> a }

instance Functor (Exp m) where
	fmap f (Exp g) = Exp (f . g)

instance Monoid m => Copointed (Exp m) where
	extract (Exp f) = f mempty

instance Monoid m => Comonad (Exp m) where
	duplicate f = Exp $ \m -> Exp $ runExp f . mappend m

