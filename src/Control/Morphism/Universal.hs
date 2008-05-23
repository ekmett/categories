-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Morphism.Universal
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD3
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-- Note the choice of which is universal and which is couniversal is chosen to 
-- make the definitions consistent with limits and colimits.

-------------------------------------------------------------------------------------------

module Control.Morphism.Universal
	( Couniversal(..), extractCouniversal, couniversalize
	, couniversalIdentity
	, Universal(..), extractUniversal, universalize
	, universalIdentity
	) where

import Control.Monad.Identity

data Couniversal a f x = Couniversal (a -> f x) (forall z. (a -> f z) -> x -> z)

extractCouniversal :: Couniversal a f x -> a -> f x
extractCouniversal (Couniversal f _) = f

couniversalize :: (a -> f z) -> Couniversal a f x -> x -> z
couniversalize f (Couniversal _ s) = s f

couniversalIdentity :: Couniversal a Identity a 
couniversalIdentity = Couniversal Identity (runIdentity .)

data Universal a f x = Universal (f x -> a) (forall z. (f z -> a) -> z -> x)

extractUniversal :: Universal a f x -> f x -> a
extractUniversal (Universal f _) = f

universalize :: Universal a f x -> (f z -> a) -> z -> x
universalize (Universal _ s) f = s f 

universalIdentity :: Universal a Identity a 
universalIdentity = Universal runIdentity (. Identity)

