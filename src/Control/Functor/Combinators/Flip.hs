-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Combinators.Flip
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD3
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-------------------------------------------------------------------------------------------

module Control.Functor.Combinators.Flip
	( Flip(..)
	, liftFlip
	) where

import Control.Monad.Identity
import Control.Functor
import Control.Category.Hask
import Control.Category.Associative
import Control.Category.Monoidal

newtype Flip p a b = Flip { runFlip :: p b a } 

liftFlip :: (p a b -> p c d) -> Flip p b a -> Flip p d c
liftFlip f = Flip . f . runFlip

instance PFunctor p Hask Hask => QFunctor (Flip p) Hask Hask where
	second g = liftFlip (first g)

instance QFunctor p Hask Hask => PFunctor (Flip p) Hask Hask where
	first f = liftFlip (second f)

instance Bifunctor p Hask Hask Hask => Bifunctor (Flip p) Hask Hask Hask where
	bimap f g = liftFlip (bimap g f)

instance Braided Hask p => Braided Hask (Flip p) where
	braid = liftFlip braid

instance Symmetric Hask p => Symmetric Hask (Flip p) 

instance Bifunctor p Hask Hask Hask => Functor (Flip p a) where
	fmap = bimap id

instance HasIdentity Hask p i => HasIdentity Hask (Flip p) i where

instance Associative Hask p => Coassociative Hask (Flip p) where
	coassociate = Flip . second Flip . associate . first runFlip . runFlip 
	-- Flip p a (Flip p b c) 	>- runFlip ->
	-- p (Flip p b c) a 		>- first runFlip ->
	-- p (p c b) a 			>- associate ->
	-- p c (p b a)			>- second Flip -> 
	-- p c (Flip p a b) 		>- Flip ->
	-- Flip p (Flip p a b) c
	
instance Coassociative Hask p => Associative Hask (Flip p) where
	associate = Flip . first Flip . coassociate . second runFlip . runFlip

instance (Coassociative Hask p, Monoidal Hask p i) => Monoidal Hask (Flip p) i where
	idl = idr . runFlip 
	idr = idl . runFlip

instance (Associative Hask p, Comonoidal Hask p i) => Comonoidal Hask (Flip p) i where
	coidl = Flip . coidr
	coidr = Flip . coidl
