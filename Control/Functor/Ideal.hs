{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Functor.Ideal
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Functor.Ideal
	( Ideal(..)
	, Coideal(..)
	, LiftI2(..)
	, toI2 
	, fromI2
	) where

import Control.Arrow ((|||),(&&&))
import Control.Bifunctor
-- import Control.Bifunctor.Biff
import Control.Functor.Bifunctor
import Control.Functor.Constant
-- import Control.Bifunctor.Fix
import Control.Monad.Identity
import Control.Applicative
import Control.Comonad




newtype LiftI2 p m n a = LiftI2 { lowerI2 :: LiftF2 p (M p m n) (N p m n) a } deriving (Functor)

fromI2 :: LiftI2 p m n a -> p (M p m n a) (N p m n a)
fromI2 = lowerF2 . lowerI2

toI2 :: p (M p m n a) (N p m n a) -> LiftI2 p m n a 
toI2 = LiftI2 . LiftF2 

data M p m n a = M { runM :: m (p a (N p m n a)) } 
data N p m n a = N { runN :: n (p a (M p m n a)) }

-- | Ideal Monads
newtype Ap p f a = Ap { runAp :: p a (f a) } 

instance (Bifunctor p, Functor f) => Functor (Ap p f) where
	fmap f = Ap . bimap f (fmap f) . runAp

instance Functor f => Pointed (Ap Either f) where
	point = Ap . Left


-- this only really needs 'ap' but there is no 'unpointed/pre- applicative'
instance Applicative f => Applicative (Ap Either f) where
	pure = point
	Ap (Left f) <*> Ap (Left a) = Ap $ Left (f a)
	Ap (Left f) <*> Ap (Right bs) = Ap $ Right (fmap f bs)
	Ap (Right fs) <*> Ap (Left a) = Ap $ Right (fmap ($a) fs)
	Ap (Right fs) <*> Ap (Right bs) = Ap $ Right (fs <*> bs)

instance (Bifunctor p, Functor m, Functor n) => Functor (M p m n) where
	fmap f = M . fmap (bimap f (fmap f)) . runM

instance (Bifunctor p, Functor m, Functor n) => Functor (N p m n) where
	fmap f = N . fmap (bimap f (fmap f)) . runN

newtype Ideal f a = Ideal { runIdeal ::  Either a (f a) }
newtype Coideal f a = Coideal { runCoideal :: (a, f a) }

class Functor w => ComonadCoideal w where
	coidealize :: w a -> w (a, w a)

instance Functor f => Functor (Coideal f) where
	fmap f = Coideal . bimap f (fmap f) . runCoideal

instance Functor f => Copointed (Coideal f) where
	extract = fst . runCoideal

instance ComonadCoideal w => Comonad (Coideal w) where
	extend f = fmap (f . Coideal) . Coideal . (id &&& coidealize . snd) . runCoideal

instance (ComonadCoideal w, ComonadCoideal v) => ComonadCoideal (LiftI2 (,) w v) where
	coidealize = undefined

{-
instance (ComonadCoideal w, ComonadCoideal v) => ComonadCoideal (LiftI2 (,) w v) where
	-- coidealize :: CoidealProduct w v a -> CoidealProduct w v (a , CoidealProduct w v a)
	coidealize = undefined
-}

-- Coideal (ConstantF k) rederives the reader comonad
instance ComonadCoideal (ConstantF k) where
	coidealize (ConstantF k) = ConstantF k
