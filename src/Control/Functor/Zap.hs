{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Zap
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (functional-dependencies)
--
-- Dual (bi)Functors
-------------------------------------------------------------------------------------------

module Control.Functor.Zap 
	( Zap(..), (>$<)
	, Bizap(..), (>>$<<)
	) where

import Control.Functor.Internal.Adjunction
