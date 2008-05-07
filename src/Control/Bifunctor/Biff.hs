-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Bifunctor.Biff
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD3
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-------------------------------------------------------------------------------------------

module Control.Bifunctor.Biff 
	( BiffB(..)
	) where

import Control.Arrow ((|||),(&&&))
import Control.Monad.Identity
import Control.Bifunctor.Monoidal
import Control.Functor.Extras
import Control.Monad.Parameterized
import Control.Comonad.Parameterized

newtype BiffB p f g a b = BiffB { runBiffB :: p (f a) (g b) } 

instance (Functor f, Bifunctor p, Functor g) => Bifunctor (BiffB p f g) where
	bimap f g = BiffB . bimap (fmap f) (fmap g) . runBiffB

instance (Functor f, Braided p) => Braided (BiffB p f f) where
	braid = BiffB . braid . runBiffB

instance (Functor f, Symmetric p) => Symmetric (BiffB p f f) 

instance (Functor f, Bifunctor p, Functor g) => Functor (BiffB p f g a) where
	fmap f = bimap id f

instance FunctorPlus f => PPointed (BiffB (,) Identity f) where
        preturn a = BiffB (Identity a,fzero)

instance Functor f => PPointed (BiffB Either Identity f) where
        preturn = BiffB . Left . Identity

instance Functor f => PCopointed (BiffB (,) Identity f) where
        pextract = runIdentity . fst . runBiffB

instance Functor f => PApplicative (BiffB Either Identity f) where
        pap = papPMonad

instance Functor f => PMonad (BiffB Either Identity f) where
        pbind k = (k . runIdentity ||| BiffB . Right) . runBiffB

instance FunctorPlus f => PApplicative (BiffB (,) Identity f) where
        pap = papPMonad

instance FunctorPlus f => PMonad (BiffB (,) Identity f) where
        pbind k (BiffB ~(Identity a,as)) = BiffB (ib, fplus as bs) where BiffB (ib,bs) = k a

instance Functor f => PComonad (BiffB (,) Identity f) where
        pextend f = BiffB . (Identity . f &&& snd . runBiffB)
