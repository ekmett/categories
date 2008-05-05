-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Zip
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-------------------------------------------------------------------------------------------

module Control.Functor.Zip where

import Control.Arrow ((&&&))
import Control.Bifunctor
import Control.Bifunctor.Composition
import Control.Bifunctor.Pair -- Bifunctor (,)
import Control.Bifunctor.Fix
import Control.Comonad.Cofree
import Control.Monad.Free
import Control.Monad.Identity
import Data.Monoid

unfzip :: Functor f => f (a, b) -> (f a, f b)
unfzip = fmap fst &&& fmap snd

unbizip :: Bifunctor p => p (a, c) (b, d) -> (p a b, p c d)
unbizip = bimap fst fst &&& bimap snd snd

{- | Minimum definition:

1. fzipWith

2. fzip

-}

class Functor f => Zip f where
	fzip :: f a -> f b -> f (a, b)
	fzip = fzipWith (,)
	fzipWith :: (a -> b -> c) -> f a -> f b -> f c
	fzipWith f as bs = fmap (uncurry f) (fzip as bs)

{- | Minimum definition: 

1. bizipWith

2. bizip

-}

class Bifunctor p => Bizip p where
	bizip :: p a c -> p b d -> p (a,b) (c,d)
	bizip = bizipWith (,) (,)
	bizipWith :: (a -> b -> e) -> (c -> d -> f) -> p a c -> p b d -> p e f 
	bizipWith f g as bs = bimap (uncurry f) (uncurry g) (bizip as bs)

instance Zip Identity where
	fzipWith f (Identity a) (Identity b) = Identity (f a b)

instance Zip [] where
	fzip = zip
	fzipWith = zipWith

instance Zip Maybe where
	fzipWith f (Just a) (Just b) = Just (f a b)
	fzipWith f _ _ = Nothing

instance Monoid a => Zip ((,)a) where
	fzipWith f (a, c) (b, d) = (mappend a b, f c d)

instance Bizip (,) where 
	bizipWith f g (a,b) (c,d) = (f a c, g b d)

-- comes for free with BiffB
-- instance Zip f => Bizip (CofreeB f) where
--	bizipWith f g (CofreeB as) (CofreeB bs) = CofreeB $ bizipWith f (fzipWith g) as bs

instance (Bizip p, Zip f, Zip g) => Bizip (BiffB p f g) where
	bizipWith f g as bs = BiffB $ bizipWith (fzipWith f) (fzipWith g) (runBiffB as) (runBiffB bs)

instance (Zip f, Bizip p) => Bizip (FunctorB f p) where
	bizipWith f g as bs = FunctorB $ fzipWith (bizipWith f g) (runFunctorB as) (runFunctorB bs)

instance Bizip p => Zip (FixB p) where
	fzipWith f as bs = InB $ bizipWith f (fzipWith f) (outB as) (outB bs)

instance Monoid a => Zip (Either a) where
	fzipWith f (Left a) (Left b) = Left (mappend a b)
	fzipWith f (Right a) (Left b) = Left b
	fzipWith f (Left a) (Right b) = Left a
	fzipWith f (Right a) (Right b) = Right (f a b)


{- -- fails because Either cannot be made an instance of Bizip!
instance Zip f => Bizip (FreeB f) where
	bizipWith f g (FreeB as) (FreeB bs) = FreeB $ bizipWith f (fzipWith g) as bs
-}
