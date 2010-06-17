{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad.Reader
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- If you look at the reader arrow:
-- @(e, a) -> a@ you can see that all the interesting bits are bunched
-- on the left. This is that comonad. Flipping the pair and currying the 
-- arguments yields @a -> (e -> a)@, and you can recognize the (e -> a) as 
-- the reader monad. In more technical language the Reader comonad is 
-- left adjoint to the Reader monad.
----------------------------------------------------------------------------
module Control.Comonad.Reader 
	( Coreader(..)
	, runCoreader
	, CoreaderT(..)
	, ComonadReader(..)
	) where

import Control.Arrow ((&&&))
import Control.Functor
import Control.Category.Hask
import Control.Comonad
import Control.Monad.Instances

class Comonad w => ComonadReader r w | w -> r where
        askC :: w a -> r

data Coreader r a = Coreader r a 

runCoreader :: Coreader r a -> (r, a)
runCoreader (Coreader r a) = (r,a)

instance ComonadReader r (Coreader r) where
	askC (Coreader r _) = r

instance Functor (Coreader r) where
	fmap f = uncurry Coreader . second f . runCoreader

instance Copointed (Coreader r) where
	extract (Coreader _ a) = a

instance Comonad (Coreader r) where
	duplicate (Coreader e a) = Coreader e (Coreader e a)

instance PFunctor Coreader Hask Hask where
	first = first'

instance QFunctor Coreader Hask Hask where
	second = second'

instance Bifunctor Coreader Hask Hask Hask where
	bimap f g = uncurry Coreader . bimap f g . runCoreader


newtype CoreaderT w r a = CoreaderT { runCoreaderT :: w (r, a) }

instance Comonad w => ComonadReader r (CoreaderT w r) where
	askC = fst . extract . runCoreaderT

instance Functor f => Functor (CoreaderT f b) where
        fmap f = CoreaderT . fmap (fmap f) . runCoreaderT

instance Copointed w => Copointed (CoreaderT w b) where
        extract = snd . extract . runCoreaderT

instance Comonad w => Comonad (CoreaderT w b) where
        duplicate = CoreaderT . liftW (fst . extract &&& CoreaderT) . duplicate . runCoreaderT

instance Functor f => PFunctor (CoreaderT f) Hask Hask where
	first = first'

instance Functor f => QFunctor (CoreaderT f) Hask Hask where
	second = second'

instance Functor f => Bifunctor (CoreaderT f) Hask Hask Hask where
	bimap f g = CoreaderT . fmap (bimap f g) . runCoreaderT

instance ComonadReader e ((,)e) where
        askC = fst
