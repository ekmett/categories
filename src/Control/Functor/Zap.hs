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
-- Dual Functors
-------------------------------------------------------------------------------------------

module Control.Functor.Zap where

import Control.Comonad.Cofree
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

class BiZap p q | p -> q, q -> p where
	bizapWith :: (a -> c -> e) -> (b -> d -> e) -> p a b -> q c d -> e

	bizap :: p (a -> c) (b -> c) -> q a b -> c
	bizap = bizapWith id id

(>>$<<) :: BiZap p q => p (a -> c) (b -> c) -> q a b -> c
(>>$<<) = bizap

instance BiZap (,) Either where
	bizapWith l _ (f,_) (Left a) = l f a
	bizapWith _ r (_,g) (Right b) = r g b 

instance BiZap Either (,) where
	bizapWith l _ (Left f) (a,_) = l f a
	bizapWith _ r (Right g) (_,b) = r g b

-- instance (Functor f, Functor g, Zap f g) => BiZap (CofreeB f) (FreeB g) where
--	bizapWith l r (CofreeB fs) (FreeB as) = bizapWith l (zapWith r) fs as

-- instance (Functor f, Functor g, Zap f g) => BiZap (FreeB f) (CofreeB g) where
--	bizapWith l r (FreeB fs) (CofreeB as) = bizapWith l (zapWith r) fs as

instance (BiZap p q, Zap f g, Zap i j) => BiZap (BiffB p f i) (BiffB q g j) where
	bizapWith l r fs as = bizapWith (zapWith l) (zapWith r) (runBiffB fs) (runBiffB as)
