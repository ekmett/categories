-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Monad.Composition
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (functional-dependencies)
--
-------------------------------------------------------------------------------------------

module Control.Monad.Composition where

import Control.Monad
import Control.Functor.Composition
import Control.Functor.Composition.Class
import Control.Functor.Extras
import Control.Functor.Pointed
import Control.Functor.Pointed.Composition

postJoin :: (Monad m, PostFold m f) => m (f (m (f a))) -> m (f a)
postJoin = join . liftM postFold

instance (Monad m, Pointed f, PostFold m f) => Monad (PostCompF m f) where
        return = compose . return . point
	m >>= k = undefined -- TODO: dualize the comonad version

preJoin :: (Monad m, Functor f, PreFold f m) => f (m (f (m a))) -> f (m a)
preJoin = fmap join . preFold

instance (Pointed f, Monad m, PreFold f m) => Monad (PreCompF f m) where
	return = compose . point . return
	m >>= k = undefined


instance (Monad m, Monad n, Distributes m n) => Monad (DistCompF m n) where
	return = compose . return . return
	m >>= k = undefined



