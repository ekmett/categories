{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Ideal
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Monad.Ideal
	( 
	-- * Ideal Monads
	  MonadIdeal(..)
	, Ideal
	, ideal
	, destroyIdeal
	-- * Coideal Comonads
	, ComonadCoideal(..)
	, Coideal
	, coideal
	, buildCoideal
	-- * Mutual recursion for (co)ideal (co)monad (co)products
	, Mutual(..)
	-- * Coideal Comonad Product
	, (:*)
	-- * Ideal Monad Coproduct
	, (:+)
	) where

import Prelude hiding (fst, snd)
import Control.Category.Cartesian 
import Control.Category.Hask
import Control.Comonad
import Control.Functor
import Control.Functor.Algebra
import Control.Functor.Combinators.Lift
import Control.Monad.Identity
-- Control.Arrow ((|||),(&&&))
-- import Control.Functor.Combinators.Biff
-- import Control.Functor.Combinators.Join
-- import Control.Applicative

type Ideal = Ap Either 
-- type Ideal f = Join (PFree f)
type Coideal = Ap (,)
-- type Coideal f = Join (PCofree f)

ideal :: Either a (f a) -> Ideal f a
ideal = mkAp

coideal :: (a, f a) -> Coideal f a 
coideal = mkAp

runIdeal :: Ideal f a -> Either a (f a)
runIdeal = runAp

runCoideal :: Coideal f a -> (a, f a)
runCoideal = runAp

class Functor m => MonadIdeal m where
	idealize :: m (Either a (m a)) -> m a

instance Functor f => Pointed (Ideal f) where
	point = Lift . Left . Identity

-- this only really needs 'ap' but there is no 'unpointed/pre- applicative'
{-
instance Applicative f => Applicative (Ideal f) where
	pure = point
	Ideal (Left f) <*> Ideal (Left a) = Ideal $ Left (f a)
	Ideal (Left f) <*> Ideal (Right bs) = Ideal $ Right (fmap f bs)
	Ideal (Right fs) <*> Ideal (Left a) = Ideal $ Right (fmap ($a) fs)
	Ideal (Right fs) <*> Ideal (Right bs) = Ideal $ Right (fs <*> bs)
-}

instance MonadIdeal m => Monad (Ideal m) where
	return = point
	m >>= f = ideal . (id ||| Right . idealize) . runIdeal $ fmap (runIdeal . f) m

destroyIdeal :: Algebra m a -> Ideal m a -> a
destroyIdeal phi = (id ||| phi) . runIdeal 


-- instance MonadIdeal (Fst k) where
--	idealize = mkFst . runFst

class Functor w => ComonadCoideal w where
	coidealize :: w a -> w (a, w a)

instance Functor f => Copointed (Coideal f) where
	extract = runIdentity . fst . runLift

instance ComonadCoideal w => Comonad (Coideal w) where
	extend f = fmap (f . coideal) . coideal . (id &&& coidealize . snd) . runCoideal

buildCoideal :: Coalgebra m a -> a -> Coideal m a
buildCoideal phi = coideal . (id &&& phi)

-- instance ComonadCoideal (Fst k) where
--	coidealize = mkFst . runFst

-- * (Co)ideal (Co)products

newtype Mutual p m n a = Mutual { runMutual :: m (p a (Mutual p n m a)) } 
type Mutual' p m n = Lift p (Mutual p m n) (Mutual p n m)
type (m :+ n) = Mutual' Either m n
type (m :* n) = Mutual' (,) m n

instance (Bifunctor p Hask Hask Hask, Functor m, Functor n) => Functor (Mutual p m n) where
	fmap f = Mutual . fmap (bimap f (fmap f)) . runMutual

{-
instance (MonadIdeal m, MonadIdeal n) => MonadIdeal (m :+ n) where
	idealize = undefined
-}

{-
instance (ComonadCoideal w, ComonadCoideal v) => ComonadCoideal (w :* v) where
	coidealize = undefined
-}
