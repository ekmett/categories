-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Bifunctor.Pointed
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (functional-dependencies)
--
-------------------------------------------------------------------------------------------

module Control.Bifunctor.Pointed
	( PPointed(..)
	, PCopointed(..)
	) where

import Control.Bifunctor

class Bifunctor f => PPointed f where
        preturn :: a -> f a c 

class Bifunctor f => PCopointed f where
	pextract :: f a c -> a

{-# RULES
"bimap id g . preturn"     	forall g. bimap id g . preturn = preturn
"pextract . bimap id g"    	forall g. pextract . bimap id g = pextract
"preturn/pextract" 		preturn . pextract = id
"pextract/preturn" 		pextract. preturn = id
 #-}

