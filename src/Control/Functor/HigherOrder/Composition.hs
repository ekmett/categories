{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.HigherOrder.Composition
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (kind annotations, rank-2 types)
--
-- Composition of higher order functors
-------------------------------------------------------------------------------------------

module Control.Functor.HigherOrder.Composition
	( CompH(..)
	, HComposition(..)
	, hassociateComp
	, hcoassociateComp
	) where

import Control.Functor.HigherOrder

class HComposition 
	(c :: ((* -> *) -> * -> *) -> 
	      ((* -> *) -> * -> *) -> 
	      ((* -> *) -> * -> *)) where
	hcompose :: f (g x) a ->  c f g x a
	hdecompose :: c f g x a -> f (g x) a

newtype CompH 
	(f :: ((* -> *) -> * -> *))
	(g :: ((* -> *) -> * -> *)) 
	(a :: (* -> *)) (b :: *) = CompH { runCompH :: f (g a) b }

instance HComposition CompH where
	hcompose = CompH
	hdecompose = runCompH

instance (HFunctor f, HFunctor g) => HFunctor (CompH f g) where
	hfmap f = hcompose . hfmap (hfmap f) . hdecompose
	ffmap f = hcompose . hfmap liftH . ffmap f . hfmap LowerH . hdecompose

hassociateComp :: (HFunctor f, HComposition c) => c (c f g) h a b -> c f (c g h) a b
hassociateComp = hcompose . hfmap hcompose . hdecompose . hdecompose

hcoassociateComp :: (HFunctor f, HComposition c) => c f (c g h) a b -> c (c f g) h a b
hcoassociateComp = hcompose . hcompose . hfmap hdecompose . hdecompose
