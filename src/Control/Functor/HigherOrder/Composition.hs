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
	, hassociateComposition
	, hcoassociateComposition
	) where

import Control.Functor.HigherOrder

class HComposition 
	(o :: ((* -> *) -> * -> *) -> 
	      ((* -> *) -> * -> *) -> 
	      ((* -> *) -> * -> *)) where
	hcompose :: f (g h) a ->  (f `o` g) h a
	hdecompose :: (f `o` g) h a -> f (g h) a

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

instance (HFunctor f, HFunctor g, Functor h) => Functor (CompH f g h) where
	fmap = ffmap

hassociateComposition :: (HFunctor f, HComposition o) => ((f `o` g) `o` h) a b -> (f `o` (g `o` h)) a b
hassociateComposition = hcompose . hfmap hcompose . hdecompose . hdecompose

hcoassociateComposition :: (HFunctor f, HComposition o) => (f `o` (g `o` h)) a b -> ((f `o` g) `o` h) a b
hcoassociateComposition = hcompose . hcompose . hfmap hdecompose . hdecompose
