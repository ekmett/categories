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
-- Described in <http://comonad.com/reader/2008/zipping-and-unzipping-functors/> and
-- <http://comonad.com/reader/2008/cozipping/>
-------------------------------------------------------------------------------------------

module Control.Functor.Zip 
	( unfzip, unbizip
	, counzip, counbizip
	, Zip(..)
	, Bizip(..)
	, Cozip(..)
	) where

import Control.Arrow ((&&&),(|||))
import Control.Bifunctor.Functor
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
	fzipWith _ _ _ = Nothing

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
	fzipWith _ (Left a) (Left b) = Left (mappend a b)
	fzipWith _ (Right _) (Left b) = Left b
	fzipWith _ (Left a) (Right _) = Left a
	fzipWith f (Right a) (Right b) = Right (f a b)


{- -- fails because Either cannot be made an instance of Bizip!
instance Zip f => Bizip (FreeB f) where
	bizipWith f g (FreeB as) (FreeB bs) = FreeB $ bizipWith f (fzipWith g) as bs
-}

counzip :: Functor f => Either (f a) (f b) -> f (Either a b)
counzip = fmap Left ||| fmap Right
 
counbizip :: Bifunctor f => Either (f a c) (f b d) -> f (Either a b) (Either c d)
counbizip = bimap Left Left ||| bimap Right Right

class Functor f => Cozip f where
   cozip :: f (Either a b) -> Either (f a) (f b)
 
instance Cozip Identity where
   cozip = bimap Identity Identity . runIdentity

instance Cozip ((,)c) where
   cozip (c,ab) = bimap ((,)c) ((,)c) ab
 
-- ambiguous choice
instance Cozip Maybe where
   cozip = maybe (Left Nothing) (bimap Just Just)
-- cozip = maybe (Right Nothing) (bimap Just Just)
 
-- ambiguous choice
instance Cozip (Either c) where
   cozip = (Left . Left) ||| bimap Right Right
-- cozip = (Right . Left) ||| bimap Right Right
