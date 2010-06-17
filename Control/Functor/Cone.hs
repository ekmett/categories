-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Functor.Cone
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism/existentials)
--
----------------------------------------------------------------------------
module Control.Functor.Cone
	( Cone, Cocone(..)
	) where

import Control.Monad.Reader
import Control.Functor.Limit

type Cone n f = n -> forall a. f a

newtype Cocone f n = Cocone { runCocone :: forall a. f a -> n }

instance Functor (Cocone f) where
	fmap f (Cocone g) = Cocone (f . g)

instance Monad (Cocone f) where
	return x = Cocone (\_ -> x)
	Cocone r >>= f = Cocone (\e -> runCocone (f (r e)) e)

instance MonadReader (Colimit f) (Cocone f) where
	ask = Cocone Colimit
	local f (Cocone r) = Cocone (\e -> case f (Colimit e) of Colimit e' -> r e')
