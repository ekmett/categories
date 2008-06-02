{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Adjunction
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (functional-dependencies)
--
-------------------------------------------------------------------------------------------

module Control.Functor.Adjunction 
	( Adjunction (unit, counit, leftAdjunct, rightAdjunct)
	, ACompF(ACompF)
	-- * Every Right Adjoint is Representable 
	, repAdjunction, unrepAdjunction
	) where

import Control.Functor.Internal.Adjunction
