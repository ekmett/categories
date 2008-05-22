-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Yoneda
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-- The Yoneda lemma materialized as a Kan extension, and hence as a higher order functor
-------------------------------------------------------------------------------------------

module Control.Functor.Yoneda
	( Yoneda
	, toYoneda, fromYoneda
	) where

import Control.Functor.KanExtension
import Control.Functor.Extras
import Control.Monad.Identity

type Yoneda = Ran Identity

toYoneda :: Functor f => f :~> Yoneda f
toYoneda a = Ran (\f -> fmap (runIdentity . f) a)

fromYoneda :: Yoneda f :~> f 
fromYoneda t = runRan t Identity

-- newtype Yoneda f a = Yoneda { runYoneda :: forall b. (a -> b) -> f b } 
-- instance HFunctor Yoneda where
--	ffmap f (Yoneda t) = check (t f)
--	hfmap f (Yoneda t) = Yoneda (f . t)
