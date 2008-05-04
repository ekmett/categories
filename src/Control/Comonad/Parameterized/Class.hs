{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Parameterized.Class
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Comonad.Parameterized.Class where

import Control.Arrow ((|||), (+++))
import Control.Monad
import Control.Bifunctor

class Bifunctor f => PComonad f where
	pextract :: f a c -> a
	pextend :: (f b c -> a) -> f b c -> f a c

{- Parameterized comonad laws:

> pextend pextract = id
> pextract . pextend g = g
> pextend (g . pextend j) = pextend g . pextend j
> pextract . second g = pextract 
> second g . pextend (j . second g) = pextend j . second g 

-}

#ifndef __HADDOCK__
{-# RULES
"pextend pextract" 		pextend pextract = id
"pextract . pextend g" 		forall g. pextract . pextend g = g
"pextract . bimap id g" 	forall g. pextract . bimap id g = pextract
"bimap _ _ . pextract" 		forall j g. bimap id g . pextend (j . bimap id g) = pextend j . bimap id g
 #-}
#endif
