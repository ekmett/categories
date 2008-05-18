-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Combinators.Biff
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD3
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-------------------------------------------------------------------------------------------

module Control.Functor.Combinators.Biff 
	( Biff(..)
	-- Parameterized Type level 'On'
	, On, runOn, mkOn
	-- Parameterized Type Level 'Ap'
	, PAp, runPAp, mkPAp
	-- Parameterized Cofree Comonad
	, PCofree, runPCofree, pcofree
	-- Parameterized Free Monad
	, PFree, runPFree, pfree
	) where

import Control.Category.Hask
import Control.Arrow ((|||),(&&&))
import Control.Monad.Identity
import Control.Category.Monoidal
import Control.Functor
import Control.Functor.Extras
import Control.Monad.Parameterized
import Control.Comonad.Parameterized

newtype Biff p f g a b = Biff { runBiff :: p (f a) (g b) } 

type PAp p = Biff p Identity

runPAp :: PFunctor p Hask Hask => PAp p f a b -> p a (f b)
runPAp = first runIdentity . runBiff

mkPAp :: PFunctor p Hask Hask => p a (f b) -> PAp p f a b
mkPAp = Biff . first Identity
 
type PFree = PAp Either

pfree :: Either a (f b) -> PFree f a b
pfree = mkPAp

runPFree :: PFree f a b -> Either a (f b)
runPFree = runPAp

type PCofree = PAp (,)

runPCofree :: PCofree f a b -> (a, f b)
runPCofree = runPAp

pcofree :: (a, f b) -> PCofree f a b
pcofree = mkPAp

type On p f = Biff p f f

runOn :: On p f a b -> p (f a) (f b)
runOn = runBiff

mkOn :: p (f a) (f b) -> On p f a b
mkOn = Biff

{-
type Joker = Biff (,) VoidF
type Clown f = Biff (,) f VoidF
type Fst = Biff (,) VoidF Identity
type Snd = Biff (,) Identity VoidF
-}

instance (Functor f, PFunctor p Hask Hask) => PFunctor (Biff p f g) Hask Hask where
	first f = Biff . first (fmap f) . runBiff

instance (QFunctor q Hask Hask, Functor g) => QFunctor (Biff q f g) Hask Hask where
	second g = Biff . second (fmap g) . runBiff

instance (Functor f, Bifunctor p Hask Hask Hask, Functor g) => Bifunctor (Biff p f g) Hask Hask Hask where
	bimap f g = Biff . bimap (fmap f) (fmap g) . runBiff

instance (Functor f, Braided p Hask) => Braided (Biff p f f) Hask where
	braid = Biff . braid . runBiff

instance (Functor f, Symmetric p Hask) => Symmetric (Biff p f f) Hask

instance (Functor f, Bifunctor p Hask Hask Hask, Functor g) => Functor (Biff p f g a) where
	fmap f = bimap id f

instance FunctorPlus f => PPointed (PCofree f) where
        preturn a = Biff (Identity a,fzero)

instance Functor f => PPointed (PFree f) where
        preturn = Biff . Left . Identity

instance Functor f => PCopointed (PCofree f) where
        pextract = runIdentity . fst . runBiff

instance Functor f => PApplicative (PFree f) where
        pap = papPMonad

instance Functor f => PMonad (PFree f) where
        pbind k = (k . runIdentity ||| Biff . Right) . runBiff

instance FunctorPlus f => PApplicative (PCofree f) where
        pap = papPMonad

instance FunctorPlus f => PMonad (PCofree f) where
        pbind k (Biff ~(Identity a,as)) = Biff (ib, fplus as bs) where Biff (ib,bs) = k a

instance Functor f => PComonad (PCofree f) where
        pextend f = Biff . (Identity . f &&& snd . runBiff)
