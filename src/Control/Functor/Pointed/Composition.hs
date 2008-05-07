{-# OPTIONS_GHC -fglasgow-exts #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Pointed.Composition
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (functional-dependencies)
--
-- TODO: finish the monad instances
-------------------------------------------------------------------------------------------

module Control.Functor.Pointed.Composition 
	( PointedCompF(..)
	, PostCompF(..)
	, PreCompF(..)
	, DistCompF(..)
	) where

import Control.Functor.Extras
import Control.Functor.Composition
import Control.Comonad
import Control.Monad
import Control.Functor.Exponential
import Control.Functor.Full

newtype PointedCompF f g a = PointedCompF (CompF f g a) deriving (Functor, ExpFunctor, Full, Composition)

instance (Pointed f, Pointed g) => Pointed (PointedCompF f g) where
        point = compose . point . point

instance (Copointed f, Copointed g) => Copointed (PointedCompF f g) where
        extract = extract . extract . decompose

newtype PostCompF mw f a = PostCompF (PointedCompF mw f a) deriving (Functor, ExpFunctor, Full, Composition, Pointed, Copointed)

instance (Comonad w, Copointed f, PostUnfold w f) => Comonad (PostCompF w f) where
        duplicate = compose . liftW (fmap compose . postUnfold) . duplicate . decompose

{-
instance (Monad m, Pointed f, PostFold m f) => Monad (PostCompF m f) where
        return = compose . return . point
        m >>= k = undefined where
		postJoin :: (Monad m, PostFold m f) => m (f (m (f a))) -> m (f a)
		postJoin = join . liftM postFold
-}


newtype PreCompF f mw a  = PreCompF (PointedCompF f mw a) deriving (Functor, ExpFunctor, Full, Composition, Pointed, Copointed)

instance (Copointed f, Comonad w, PreUnfold f w) => Comonad (PreCompF f w) where
        duplicate = compose . fmap (liftW compose) . preUnfold . fmap (duplicate) . decompose

{-
instance (Pointed f, Monad m, PreFold f m) => Monad (PreCompF f m) where
        return = compose . point . return
        m >>= k = undefined where
		preJoin :: (Monad m, Functor f, PreFold f m) => f (m (f (m a))) -> f (m a)
		preJoin = fmap join . preFold
-}


newtype DistCompF f g a  = DistCompF (PointedCompF f g a) deriving (Functor, ExpFunctor, Full, Composition, Pointed, Copointed)

instance (Comonad f, Comonad g, Distributes f g) => Comonad (DistCompF f g) where
        duplicate = compose . fmap (fmap compose . dist) . duplicate . fmap duplicate . decompose

{-
instance (Monad m, Monad n, Distributes m n) => Monad (DistCompF m n) where
        return = compose . return . return
        m >>= k = undefined
-}
