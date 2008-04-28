-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Full
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-------------------------------------------------------------------------------------------

module Control.Functor.Full where

{- |
	A 'Full' 'Functor' @F : C -> D@ provides for every pair of objects @c@, @c'@ in @C@
	and every morphism @g : F c -> F c'l@ in @D@, a morphism @g' : c -> c'@ in @C@. In short
	map has a right-inverse under composition.

> fmap . prefmap = id
-}

class Functor f => Full f where
	prefmap :: (f a -> f b) -> a -> b
	
{-# RULES
	"fmap/prefmap" 	map . prefmap = id
 #-}

class Functor f => Faithful f

{- | 

For every pair of objects @a@ and @b@ in @C@ a 'Full' 'Faithful' 'Functor' @F : C -> D@ maps every morphism 
@f : a -> b@ onto a distinct morphism @f : T a -> T b@ (since it is faithful) and every morphism from 
@g : T a -> T b@ can be obtained from some @f@. (It maps Hom-sets bijectively, or in short @fmap@ has both
a left and right inverse under composition.

> unfmap . fmap = id
-}

unfmap :: (Full f, Faithful f) => (f a -> f b) -> a -> b
unfmap = prefmap

{-# RULES
	"unfmap/fmap"	unfmap . fmap = id
 #-}
