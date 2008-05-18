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

import Control.Functor.Combinators.Biff
import Control.Monad.Either ()
import Control.Monad.Identity

{- | Minimum definition: zapWith -}

class Zap f g | f -> g, g -> f where
	zapWith :: (a -> b -> c) -> f a -> g b -> c
	zap :: f (a -> b) -> g a -> b
	zap = zapWith id

(>$<) :: Zap f g => f (a -> b) -> g a -> b
(>$<) = zap

instance Zap Identity Identity where
	zapWith f (Identity a) (Identity b) = f a b

{- | Minimum definition: bizapWith -}

class Bizap p q | p -> q, q -> p where
	bizapWith :: (a -> c -> e) -> (b -> d -> e) -> p a b -> q c d -> e

	bizap :: p (a -> c) (b -> c) -> q a b -> c
	bizap = bizapWith id id

(>>$<<) :: Bizap p q => p (a -> c) (b -> c) -> q a b -> c
(>>$<<) = bizap

instance Bizap (,) Either where
	bizapWith l _ (f,_) (Left a) = l f a
	bizapWith _ r (_,g) (Right b) = r g b 

instance Bizap Either (,) where
	bizapWith l _ (Left f) (a,_) = l f a
	bizapWith _ r (Right g) (_,b) = r g b

instance (Bizap p q, Zap f g, Zap i j) => Bizap (Biff p f i) (Biff q g j) where
	bizapWith l r fs as = bizapWith (zapWith l) (zapWith r) (runBiff fs) (runBiff as)
