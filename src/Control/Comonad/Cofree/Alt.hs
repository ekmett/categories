{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Cofree.Alt
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  rank-2 types 
--
----------------------------------------------------------------------------
module Control.Comonad.Cofree.Alt where

import Control.Comonad
import Control.Arrow ((|||), (&&&), (+++), (***))

data Cofree' f c = forall a. Cofree' (a -> c) (a -> f a) a

runCofree' :: Functor f => Cofree' f c -> (c, f (Cofree' f c))
runCofree' = extract &&& outCofree'

instance Functor (Cofree' f) where
	fmap f (Cofree' h t s) = Cofree' (f . h) t s

instance Comonad (Cofree' f) where	
	extract (Cofree' h _ s ) = h s 
	extend f (Cofree' h t s) = Cofree' (f . Cofree' h t) t s

outCofree' :: Functor f => Cofree' f a -> f (Cofree' f a)
outCofree' (Cofree' h t s) = fmap (anaCofree' h t) (t s)

anaCofree' :: (a -> c) -> (a -> f a) -> a -> Cofree' f c
anaCofree' = Cofree'
