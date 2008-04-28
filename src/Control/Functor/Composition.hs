{-# OPTIONS -fglasgow-exts #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Composition
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-- Generalized functor composition.
-------------------------------------------------------------------------------------------

module Control.Functor.Composition where

import Control.Bifunctor
import Control.Bifunctor.Instances
import Control.Functor.Adjunction
import Control.Functor.Pointed
import Control.Monad
import Control.Comonad
import Control.Functor.Full
import Control.Arrow ((&&&),(|||))

newtype CompF f g a = CompF { deCompF :: f (g a) }

#ifndef __HADDOCK__
type (f :.: g) a = CompF f g a
#endif

instance Adjunction f g => Pointed (CompF g f) where
        point = CompF . unit

instance Adjunction f g => Copointed (CompF f g) where
        copoint = counit . deCompF

instance Adjunction f g => Monad (CompF g f) where
	return = point
        m >>= f = CompF . fmap (rightAdjunct (deCompF . f)) $ deCompF m

instance Adjunction f g => Comonad (CompF f g) where
	extract = copoint
        extend f = CompF . fmap (leftAdjunct (f . CompF)) . deCompF

instance (Functor f, Functor g) => Functor (CompF f g) where
	fmap f = CompF . fmap (fmap f) . deCompF

instance (Full f, Full g) => Full (CompF f g) where
        prefmap f = prefmap . prefmap $ deCompF . f . CompF

newtype BifunctorF p f g a = BifunctorF { runBifunctorF :: p (f a) (g a) }

instance (Bifunctor p, Functor f ,Functor g) => Functor (BifunctorF p f g) where
	fmap f = BifunctorF . bimap (fmap f) (fmap f) . runBifunctorF

#ifndef __HADDOCK__
type (f :*: g) a = BifunctorF (,) f g a
#endif

-- this would be a type but that causes the following line to freak out
-- because (f :*: g) is a partialy applied type synonym even though it immediately gets filled out

#ifndef __HADDOCK__
runProductF :: (f * g) a -> (f a, g a)
#else
runProductF :: Bifunctor (,) f g a -> (f a, g a)
#endif
runProductF = runBifunctorF

instance (Pointed f, Pointed g) => Pointed (BifunctorF (,) f g) where
	point = BifunctorF . (point &&& point)

instance (Faithful f, Faithful g) => Faithful (BifunctorF (,) f g)

#ifndef __HADDOCK__
type (f :+: g) a = BifunctorF Either f g a
#endif

#ifndef __HADDOCK__
runCoproductF :: (f :+: g) a -> Either (f a) (g a)
#else
runCoproductF :: Bifunctor Either f g a -> Either (f a) (g a)
#endif
runCoproductF = runBifunctorF 

instance (Copointed f, Copointed g) => Copointed (BifunctorF Either f g) where
	copoint = (copoint ||| copoint) . runCoproductF
