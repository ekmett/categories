-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Representable
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-------------------------------------------------------------------------------------------

module Control.Functor.Representable where

class Functor f => Representable f x where
	rep :: (x -> a) -> f a
	unrep :: f a -> (x -> a)

{-# RULES
  "rep/unrep"       	rep . unrep = id
  "unrep/rep"   	unrep . rep = id
 #-}
