-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD3
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (functional-dependencies)
--
-- A more categorical definition of Functor than endofunctors in the category Hask
-------------------------------------------------------------------------------------------
module Control.Functor
	( PFunctor (first), first'
	, QFunctor (second), second'
	, Bifunctor (bimap)
	) where

import Prelude hiding (id,(.))
import Control.Category
import Control.Category.Hask

class (Category r, Category t) => PFunctor p r t | p r -> t, p t -> r where
	first :: r a b -> t (p a c) (p b c)

{-# INLINE first' #-}
first' :: Bifunctor p r s t => r a b -> t (p a c) (p b c)
first' f = bimap f id

class (Category s, Category t) => QFunctor q s t | q s -> t, q t -> s where
	second :: s a b -> t (q c a) (q c b)

{-# INLINE second' #-}
second' :: Bifunctor p r s t => s a b -> t (p c a) (p c b)
second' = bimap id

instance PFunctor Either Hask Hask where
	first = first'

instance QFunctor Either Hask Hask where
	second = second'

instance Bifunctor Either Hask Hask Hask where
        bimap f _ (Left a) = Left (f a)
	bimap _ g (Right a) = Right (g a)

instance QFunctor (->) Hask Hask where
	second = (.)

instance PFunctor (,) Hask Hask where
	first = first'

instance QFunctor (,) Hask Hask where
	second = second'

instance Bifunctor (,) Hask Hask Hask where
        bimap f g ~(a,b)= (f a, g b)

class (PFunctor p r t, QFunctor p s t) => Bifunctor p r s t | p r -> s t, p s -> r t, p t -> r s where
	bimap :: r a b -> s c d -> t (p a c) (p b d)
