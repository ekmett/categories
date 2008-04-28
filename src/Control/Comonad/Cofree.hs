{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Cofree
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Comonad.Cofree where

import Control.Comonad
import Control.Arrow ((|||), (&&&), (+++), (***))

-- | The cofree comonad of a functor (aka the branching stream comonad)
data Cofree f a = Cofree { runCofree :: (a, f (Cofree f a)) }

instance Functor f => Functor (Cofree f) where
        fmap f = Cofree . (f *** fmap (fmap f)) . runCofree

instance Functor f => Comonad (Cofree f) where
        extract = fst . runCofree
        extend f = Cofree . (f &&& (fmap (extend f) . outCofree))

outCofree :: Cofree f a -> f (Cofree f a)
outCofree = snd . runCofree

anaCofree :: Functor f => (a -> c) -> (a -> f a) -> a -> Cofree f c
anaCofree h t = Cofree . (h &&& fmap (anaCofree h t) . t)
