{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Morphism.Apo
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
-- 
-- Traditional operators, shown here to show how to roll your own
----------------------------------------------------------------------------
module Control.Morphism.Apo where


import Control.Functor.Algebra
import Control.Functor.Extras
import Control.Functor.Fix
import Control.Monad
import Control.Monad.Either
import Control.Morphism.Ana
import Control.Arrow ((|||))

-- * Unfold Sugar

apo :: Functor f => CoAlgM f (Apo f) a -> a -> Fix f
apo = g_apo outF

g_apo :: Functor f => CoAlg f b -> CoAlgM f (GApo b) a -> a -> Fix f
g_apo g = g_ana (distGApo g)

type Apo f a 		= Either (Fix f) a
type ApoT f m a 	= EitherT (Fix f) m a

type GApo b a 		= Either b a
type GApoT b m a 	= EitherT b m a 

-- * Distributive Law Combinators

distGApo :: Functor f => CoAlg f b -> Dist (Either b) f
distGApo f = fmap Left . f  ||| fmap Right

distGApoT :: (Functor f, Monad m) => CoAlgM f m b -> Dist m f -> Dist (EitherT b m) f
distGApoT g k = fmap (EitherT . join) . k  . liftM (fmap (liftM Left) . g ||| fmap (return . Right)) . runEitherT

distApoT :: (Functor f, Monad m) => Dist m f -> Dist (ApoT f m) f
distApoT = distGApoT (liftCoAlg outF)

