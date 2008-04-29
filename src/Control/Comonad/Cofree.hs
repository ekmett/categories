{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Cofree
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  rank-2 types 
--
----------------------------------------------------------------------------
module Control.Comonad.Cofree where

import Control.Comonad
import Control.Functor.Pointed
import Control.Functor.Contravariant
import Control.Functor.Exponential
import Control.Arrow ((|||), (&&&), (+++), (***))

data Cofree f a = Cofree { runCofree :: (a, f (Cofree f a)) }

instance Functor f => Functor (Cofree f) where
        fmap f = Cofree . (f *** fmap (fmap f)) . runCofree

-- instance ContravariantFunctor f => ContravariantFunctor (Cofree f) where
--       contramap f = Cofree . (f *** contramap (fmap f)) . runCofree

instance ExpFunctor f => ExpFunctor (Cofree f) where
        xmap f g = Cofree . (f *** xmap (xmap f g) (xmap g f)) . runCofree

instance Functor f => Comonad (Cofree f) where
        extract = fst . runCofree
        extend f = Cofree . (f &&& (fmap (extend f) . outCofree))

instance Functor f => Copointed (Cofree f) where
	copoint = extract

outCofree :: Cofree f a -> f (Cofree f a)
outCofree = snd . runCofree

anaCofree :: Functor f => (a -> c) -> (a -> f a) -> a -> Cofree f c
anaCofree h t = Cofree . (h &&& fmap (anaCofree h t) . t)

