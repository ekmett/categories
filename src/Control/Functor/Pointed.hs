-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Pointed
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (functional-dependencies)
--
-------------------------------------------------------------------------------------------

module Control.Functor.Pointed where

-- return
class Functor f => Pointed f where
        point :: a -> f a

-- extract
class Functor f => Copointed f where
        copoint :: f a -> a

{-# RULES
        "copoint/point" copoint . point = id
        "point/copoint" point . copoint = id
 #-}
