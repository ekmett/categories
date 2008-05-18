{-# LANGUAGE TypeFamilies #-}
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

instance Braided p Hask => Braided (Flip p) Hask where
	braid = liftFlip braid

instance Symmetric p Hask => Symmetric (Flip p) Hask

instance Bifunctor p Hask Hask Hask => Functor (Flip p a) where
	fmap = bimap id

instance HasIdentity p Hask => HasIdentity (Flip p) Hask where
	type Id (Flip p) Hask = Id p Hask

instance Associative p Hask => Coassociative (Flip p) Hask where
	coassociate = Flip . second Flip . associate . first runFlip . runFlip 
	-- Flip p a (Flip p b c) 	>- runFlip ->
	-- p (Flip p b c) a 		>- first runFlip ->
	-- p (p c b) a 			>- associate ->
	-- p c (p b a)			>- second Flip -> 
	-- p c (Flip p a b) 		>- Flip ->
	-- Flip p (Flip p a b) c
	
instance Coassociative p Hask => Associative (Flip p) Hask where
	associate = Flip . first Flip . coassociate . second runFlip . runFlip

instance (Coassociative p Hask, Monoidal p Hask) => Monoidal (Flip p) Hask where
	idl = idr . runFlip 
	idr = idl . runFlip

instance (Associative p Hask, Comonoidal p Hask) => Comonoidal (Flip p) Hask where
	coidl = Flip . coidr
	coidr = Flip . coidl
