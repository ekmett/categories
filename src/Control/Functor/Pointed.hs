-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Pointed
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-------------------------------------------------------------------------------------------

module Control.Functor.Pointed 
	( Pointed(..)
	, Copointed(..)
	) where

import Control.Monad.Identity

-- return
class Functor f => Pointed f where
        point :: a -> f a

class Functor f => Copointed f where
        extract :: f a -> a

{-# RULES
"extract/point" extract . point = id
"point/extract" point . extract = id
 #-}

instance Pointed Identity where
	point = Identity

instance Pointed Maybe where
	point = Just

instance Pointed (Either a) where
	point = Right

instance Pointed [] where
	point a = [a]

instance Copointed Identity where
        extract = runIdentity

instance Copointed ((,)e) where
	extract = snd
