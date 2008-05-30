{-# OPTIONS_GHC -fglasgow-exts #-}
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

module Control.Functor.Representable 
	( Representable, rep, unrep
--	, repAdjunction, unrepAdjunction
	, Both(..), EitherF(..)
	) where

import Control.Monad.Identity

class Functor f => Representable f x where
	rep :: (x -> a) -> f a
	unrep :: f a -> (x -> a)

{-# RULES
"rep/unrep" rep . unrep = id
"unrep/rep" unrep . rep = id
 #-}

--repAdjunction :: Adjunction f g => (f () -> a) -> g a
--repAdjunction f = leftAdjunct f ()

--unrepAdjunction :: Adjunction f g => g a -> (f () -> a)
--unrepAdjunction = rightAdjunction . const

data EitherF a b c = EitherF (a -> c) (b -> c)

instance Functor (EitherF a b) where
        fmap f (EitherF l r) = EitherF (f . l) (f . r)

instance Representable (EitherF a b) (Either a b) where
        rep f = EitherF (f . Left) (f . Right)
        unrep (EitherF l r) = either l r

instance Representable Identity () where
	rep f = Identity (f ())
	unrep (Identity a) = const a

data Both a = Both a a 

instance Functor Both where
	fmap f (Both a b) = Both (f a) (f b)

instance Representable Both Bool where
	rep f = Both (f False) (f True)
	unrep (Both x _) False = x
	unrep (Both _ y) True = y

-- instance Adjunction f g => Representable g (f ()) where
-- instance Representable (Cofree Identity) (Free Identity ()) where
