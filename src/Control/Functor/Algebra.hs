-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Functor.Algebra
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
--
-- Algebras, Coalgebras, Bialgebras, and Dialgebras and their (co)monadic
-- variants
----------------------------------------------------------------------------
module Control.Functor.Algebra 
	( Dialgebra, GDialgebra
	, Bialgebra, GBialgebra
	, Algebra, GAlgebra
	, Coalgebra, GCoalgebra
	, Trialgebra
	, liftAlgebra
	, liftCoalgebra
	, liftDialgebra
	, fromCoalgebra
	, fromAlgebra
	, fromBialgebra
	) where

import Control.Comonad
import Control.Monad.Identity
import Control.Functor
import Control.Functor.Extras
import Control.Functor.Combinators.Lift

-- | F,G-dialgebras generalize algebras and coalgebraas
type Dialgebra f g a = f a -> g a

type GDialgebra f g w m a = f (w a) -> g (m a)

-- | F-G-bialgebras are representable by @DiAlg (f :+: Identity) (Identity :+: g) a@
-- and so add no expressive power, but are a lot more convenient.
type Bialgebra f g a = (Algebra f a, Coalgebra g a)
type GBialgebra f g w m a = (GAlgebra f w a, GCoalgebra g m a)

-- | Martin Erwig's trialgebras for indexed data types
type Trialgebra f g h a = (Algebra f a, Dialgebra g h a)

-- | F-Algebras
type Algebra f a = f a -> a

-- | F-Coalgebras
type Coalgebra f a = a -> f a

-- | F-W-Comonadic Algebras for a given comonad W
type GAlgebra f w a = f (w a) -> a

-- | F-M-Monadic Coalgebras for a given monad M
type GCoalgebra f m a = a -> f (m a)

-- | Turn an F-algebra into a F-W-algebra by throwing away the comonad
liftAlgebra :: (Functor f, Comonad w) => Algebra f :~> GAlgebra f w 
liftAlgebra phi = phi . fmap extract

-- | Turn a F-coalgebra into a F-M-coalgebra by returning into a monad
liftCoalgebra :: (Functor f, Monad m) => Coalgebra f :~> GCoalgebra f m
liftCoalgebra psi = fmap return . psi

liftDialgebra :: (Functor g, Functor f, Comonad w, Monad m) => Dialgebra f g :~> GDialgebra f g w m 
liftDialgebra phi = fmap return . phi . fmap extract

fromAlgebra :: Algebra f :~> Dialgebra f Identity
fromAlgebra phi = Identity . phi

fromCoalgebra :: Coalgebra f :~> Dialgebra Identity f
fromCoalgebra psi = psi . runIdentity

fromBialgebra :: Bialgebra f g :~> Dialgebra (f :*: Identity) (Identity :*: g) 
fromBialgebra (phi,psi) = Lift . bimap (Identity . phi) (psi . runIdentity) . runLift 
