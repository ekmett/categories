{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Context
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs)
--
-- The state-in-context comonad and comonad transformer
----------------------------------------------------------------------------
module Control.Comonad.Context where

import Control.Arrow ((&&&), first)
import Control.Comonad
import Control.Comonad.Context.Class


data Context s a = Context (s -> a) s

runContext f s = (a b, b) where
	Context a b = f (Context id s)

instance ComonadContext s (Context s) where
	getC (Context f s) = s
	modifyC m (Context f c) = f (m c)
	
instance Functor (Context s) where
	fmap f (Context f' s) = Context (f . f') s

instance Comonad (Context s) where
	extract   (Context f a) = f a
	duplicate (Context f a) = Context (Context f) a




newtype ContextT s w a = ContextT { runContextT :: (w s -> a, w s) }

instance Comonad w => ComonadContext s (ContextT s w) where
	getC = extract . snd . runContextT 
	modifyC m (ContextT (f,c)) = f (fmap m c)

instance Functor (ContextT b f) where
        fmap f = ContextT . first (f .) . runContextT

instance Comonad w => Comonad (ContextT b w) where
        extract = uncurry id . runContextT
        duplicate (ContextT (f,ws)) = ContextT (ContextT . (,) f, ws)
