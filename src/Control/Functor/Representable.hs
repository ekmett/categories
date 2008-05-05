{-# OPTIONS -fglasgow-exts #-}
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
"rep/unrep" rep . unrep = id
"unrep/rep" unrep . rep = id
 #-}

data EitherF a b c = EitherF (a -> c) (b -> c)

instance Functor (EitherF a b) where
        fmap f (EitherF l r) = EitherF (f . l) (f . r)

instance Representable (EitherF a b) (Either a b) where
        rep f = EitherF (f . Left) (f . Right)
        unrep (EitherF l r) = either l r

