-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Comonad.Composition
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (functional-dependencies)
--
-- composing a comonad with a copointed endofunctor yields a comonad given a distributive law
-------------------------------------------------------------------------------------------

module Control.Comonad.Composition where

import Control.Comonad
import Control.Functor.Composition
import Control.Functor.Composition.Class
import Control.Functor.Extras
import Control.Functor.Pointed
import Control.Functor.Pointed.Composition

-- postDuplicate :: (Comonad w, PostUnfold w f) => w (f a) -> w (f (w (f a)))
-- postDuplicate = fmap postUnfold . duplicate

instance (Comonad w, Copointed f, PostUnfold w f) => Comonad (PostCompF w f) where
	extract = copoint . extract . decompose
	duplicate = compose . liftW (fmap compose . postUnfold) . duplicate . decompose

-- preDuplicate :: (Comonad w, Functor f, PreUnfold f w) => f (w a) -> f (w (f (w a)))
-- preDuplicate = preUnfold . fmap duplicate

instance (Copointed f, Comonad w, PreUnfold f w) => Comonad (PreCompF f w) where
	extract = extract . copoint .  decompose
	duplicate = compose . fmap (liftW compose) . preUnfold . fmap (duplicate) . decompose

instance (Comonad f, Comonad g, Distributes f g) => Comonad (DistCompF f g) where
	extract = extract . extract . decompose
	duplicate = compose . fmap (fmap compose . dist) . duplicate . fmap duplicate . decompose
--	join = compose . fmap join . join . fmap dist . fmap (fmap decompose) . decompose
