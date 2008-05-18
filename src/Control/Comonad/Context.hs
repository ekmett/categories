{-# OPTIONS_GHC -fglasgow-exts #-}
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
-- The Context Comonad Transformer is related to the left Kan Extension 'Lan' of 
-- a comonad along itself, except the type of the context is fixed, and 
-- not existentially quantified.

-- The context comonad can more traditionally be derived from the 'hom-prod' 
-- adjunction between (->) and (,)
----------------------------------------------------------------------------
module Control.Comonad.Context 
	( module Control.Comonad
	, ComonadContext(..)
	, putC
	, experiment
	, Context(..)
	, runContext
	, ContextT(..)
	) where

import Control.Functor (first)
import Control.Comonad

class Comonad w => ComonadContext s w | w -> s where
	getC :: w a -> s
	modifyC :: (s -> s) -> w a -> a 

putC :: ComonadContext s w => s -> w a -> a
putC = modifyC . const 

experiment :: (ComonadContext s w, Functor f) => f (s -> s) -> w a -> f a
experiment ms a = fmap (flip modifyC a) ms

data Context s a = Context (s -> a) s


runContext :: (Context s s -> Context s b) -> s -> (b, s)
runContext f s = (a b, b) where
	Context a b = f (Context id s)

instance ComonadContext s (Context s) where
	getC (Context _ s) = s
	modifyC m (Context f c) = f (m c)
	
instance Functor (Context s) where
	fmap f (Context f' s) = Context (f . f') s

instance Copointed (Context s) where
	extract   (Context f a) = f a

instance Comonad (Context s) where
	duplicate (Context f a) = Context (Context f) a


newtype ContextT s w a = ContextT { runContextT :: (w s -> a, w s) }

instance Comonad w => ComonadContext s (ContextT s w) where
	getC = extract . snd . runContextT 
	modifyC m (ContextT (f,c)) = f (fmap m c)

instance Functor (ContextT b f) where
        fmap f = ContextT . first (f .) . runContextT

instance Copointed (ContextT b w) where
        extract = uncurry id . runContextT

instance Comonad w => Comonad (ContextT b w) where
        duplicate (ContextT (f,ws)) = ContextT (ContextT . (,) f, ws)
