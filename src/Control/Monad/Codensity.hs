-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Codensity
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
--
----------------------------------------------------------------------------
module Control.Monad.Codensity
	( Codensity, liftCodensity, lowerCodensity
	, codensityToRan, ranToCodensity
	, toCodensity, fromCodensity
	, codensityToAdjunction
	, adjunctionToCodensity
	, improveFree
	) where

import Prelude hiding (abs)
import Control.Comonad.Context
import Control.Functor.Extras
import Control.Functor.Pointed ()
import Control.Functor.Adjunction
import Control.Functor.KanExtension
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Free

newtype Codensity m a = Codensity { runCodensity :: forall b. (a -> m b) -> m b }

codensityToRan :: Codensity m :~> Ran m m
codensityToRan x = Ran (runCodensity x)

ranToCodensity :: Ran m m :~> Codensity m
ranToCodensity x = Codensity (runRan x)

liftCodensity :: Monad m => m :~> Codensity m
liftCodensity m = Codensity (m >>=)

lowerCodensity :: Monad m => Codensity m :~> m
lowerCodensity a = runCodensity a return

toCodensity :: Functor s => (forall a. s (k a) -> k a) -> s :~> Codensity k
toCodensity s t = Codensity (s . flip fmap t)

fromCodensity :: (s :~> Codensity k) -> s (k a) -> k a
fromCodensity s = flip runCodensity id . s

instance Functor (Codensity k) where
	fmap f m = Codensity (\k -> runCodensity m (k . f))

instance Pointed (Codensity f) where
	point x = Codensity (\k -> k x)

instance Monad (Codensity f) where
	return = point
	m >>= k = Codensity (\c -> runCodensity m (\a -> runCodensity (k a) c))

instance MonadReader r m => MonadReader r (Codensity m) where
	ask = liftCodensity ask
	local f m = Codensity (\c -> ask >>= \r -> local f (runCodensity m (local (const r) . c)))

instance MonadIO m => MonadIO (Codensity m) where
	liftIO = liftCodensity . liftIO 

instance MonadState s m => MonadState s (Codensity m) where
	get = liftCodensity get
	put = liftCodensity . put

instance MonadFree f m => MonadFree f (Codensity m) where
        inFree t = Codensity (inFree . flip fmap t . flip runCodensity)

instance RunMonadFree f m => RunMonadFree f (Codensity m) where
	cataFree l r = cataFree l r . lowerCodensity

codensityToAdjunction :: Adjunction f g => Codensity g a -> g (f a)
codensityToAdjunction r = runCodensity r unit

adjunctionToCodensity :: Adjunction f g => g (f a) -> Codensity g a
adjunctionToCodensity f = Codensity (\a -> fmap (rightAdjunct a) f)

improveFree :: Functor f => (forall m. MonadFree f m => m a) -> Free f a
improveFree m = lowerCodensity m
