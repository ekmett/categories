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
	, PPointed(..)
	, PCopointed(..)
	) where

import Control.Functor
import Control.Category
import Control.Category.Hask
import Control.Monad.Identity
import Prelude hiding ((.),id)

-- return
class Functor f => Pointed f where
        point :: a -> f a

class Functor f => Copointed f where
        extract :: f a -> a

{-# RULES
"extract/point" extract . point = id
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

class PFunctor f Hask Hask => PPointed f where
        preturn :: a -> f a c
        -- preturn :: k a (f a c)

class PFunctor f Hask Hask => PCopointed f where
	pextract :: f a c -> a
	-- pextract :: k (f a c) a

{-# RULES
"bimap id g . preturn"     	forall g. bimap id g . preturn = preturn
"pextract . bimap id g"    	forall g. pextract . bimap id g = pextract
"preturn/pextract" 		preturn . pextract = id
"pextract/preturn" 		pextract. preturn = id
 #-}
