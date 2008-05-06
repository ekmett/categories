{-# OPTIONS_GHC -cpp #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Arrow.BiKleisli
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD3
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-------------------------------------------------------------------------------------------

module Control.Arrow.BiKleisli where

#if __GLASGOW_HASKELL__ >= 609
import Prelude hiding (id,(.))
import Control.Category
#endif
import Control.Monad (liftM)
import Control.Comonad
import Control.Arrow
import Control.Functor.Extras

newtype BiKleisli w m a b = BiKleisli { runBiKleisli :: w a -> m b }

instance Monad m => Functor (BiKleisli w m a) where
	fmap f (BiKleisli g) = BiKleisli (liftM f . g)

instance (Comonad w, Monad m, Distributes w m) => Arrow (BiKleisli w m) where
	arr f = BiKleisli (return . f . extract)
	first (BiKleisli f) = BiKleisli $ \x -> do
		u <- f (fmap fst x)
		return (u, extract (fmap snd x))
#if __GLASGOW_HASKELL__ < 609
	BiKleisli g >>> BiKleisli f = BiKleisli ((>>= f) . dist . extend g)
#else
instance (Comonad w, Monad m, Distributes w m) => Category (BiKleisli w m) where
	BiKleisli f . BiKleisli g = BiKleisli ((>>=f) . dist . extend g)
	id = BiKleisli (return . extract)
#endif
