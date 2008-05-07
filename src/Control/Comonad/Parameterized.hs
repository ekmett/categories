{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Parameterized
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Comonad.Parameterized 
	( Bifunctor(..)
	, PCopointed(..)
	, PComonad(..)
	) where

import Control.Bifunctor
import Control.Bifunctor.Pointed

class PCopointed f => PComonad f where
	pextend :: (f b c -> a) -> f b c -> f a c

{- Parameterized comonad laws:

> pextend pextract = id
> pextract . pextend g = g
> pextend (g . pextend j) = pextend g . pextend j
> pextract . second g = pextract 
> second g . pextend (j . second g) = pextend j . second g 

-}

{-# RULES
"pextend pextract" 		pextend pextract = id
"pextract . pextend g" 		forall g. pextract . pextend g = g
"bimap _ _ . pextract" 		forall j g. bimap id g . pextend (j . bimap id g) = pextend j . bimap id g
 #-}

	
