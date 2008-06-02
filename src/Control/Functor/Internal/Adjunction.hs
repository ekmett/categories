{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Internal.Adjunction
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (functional-dependencies)
--
-------------------------------------------------------------------------------------------

module Control.Functor.Internal.Adjunction 
	( 
	-- * Adjunction
	  Adjunction (unit, counit, leftAdjunct, rightAdjunct)
	, ACompF(ACompF)
	, repAdjunction, unrepAdjunction
	-- * Representability
	, Representable, rep, unrep
	, Corepresentable, corep, uncorep
	, Both(..), EitherF(..)
	-- * Zapping
	, Zap(..), (>$<)
	, Bizap(..), (>>$<<)
	) where

import Control.Comonad.Reader
import Control.Comonad.Context
import Control.Functor.Combinators.Biff
import Control.Functor.Contra
import Control.Functor.Composition
import Control.Functor.Exponential
import Control.Functor.Full
import Control.Functor.Strong
import Control.Functor.HigherOrder
import Control.Applicative
import Control.Monad.Either ()
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

-- | An 'Adjunction' formed by the 'Functor' f and 'Functor' g. 

-- Minimal definition:

-- 1. @leftAdjunct@ and @rightAdjunct@ 

-- 2. @unit@ and @counit@

-- The following ambiguous instances prevent the requirement that (Zap f g, Zap g f) be 
-- a prerequisite for Adjunction:

-- instance (Adjunction f1 g1, Adjunction f2 g2) => Zap (CompF g1 g2) (CompF f2 f1) where ...
-- instance (Adjunction f1 g1, Adjunction f2 g2) => Zap (CompF f2 f1) (CompF g1 g2) where ...
-- instance (Zap f g, Zap f' g') => Zap (CompF f f') (Comp g g')
--	zapWith f a b = zapWith (zapWith f) (decompose a) (decompose b)
-- instance (Zap f g, Zap g f, Representable g (f ()), Functor f) => Adjunction f g | f -> g, g -> f where
class (Representable g (f ()), Functor f) => Adjunction f g | f -> g, g -> f where
	unit   :: a -> g (f a)
	counit :: f (g a) -> a
	leftAdjunct  :: (f a -> b) -> a -> g b
	rightAdjunct :: (a -> g b) -> f a -> b

	unit = leftAdjunct id
	counit = rightAdjunct id
	leftAdjunct f = fmap f . unit
	rightAdjunct f = counit . fmap f

zapWithGF :: Adjunction g f => (a -> b -> c) -> f a -> g b -> c
zapWithGF f a b = uncurry (flip f) . counit . fmap (uncurry (flip strength)) $ strength a b

-- more appropriate to use 'data Empty' or a (co)limit to ground out f ?
repAdjunction :: Adjunction f g => (f () -> a) -> g a
repAdjunction f = leftAdjunct f ()

unrepAdjunction :: Adjunction f g => g a -> (f () -> a)
unrepAdjunction = rightAdjunct . const

-- TODO: widen?
instance (Adjunction f1 g1, Adjunction f2 g2) => Representable (CompF g1 g2) (CompF f2 f1 ()) where
	rep = repAdjunction
	unrep = unrepAdjunction

instance (Adjunction f1 g1, Adjunction f2 g2) => Adjunction (CompF f2 f1) (CompF g1 g2) where
	counit = counit . fmap (counit . fmap decompose) . decompose
	unit = compose . fmap (fmap compose . unit) . unit

-- | Adjunction-oriented composition, yields monads and comonads from adjunctions
newtype ACompF f g a = ACompF (CompF f g a) deriving (Functor, ExpFunctor, Full, Composition, HFunctor)

instance Adjunction f g => Pointed (ACompF g f) where
        point = compose . unit

instance Adjunction f g => Copointed (ACompF f g) where
        extract = counit . decompose

instance Adjunction f g => Applicative (ACompF g f) where
	pure = point
	(<*>) = ap

instance Adjunction f g => Monad (ACompF g f) where
        return = point
        m >>= f = compose . fmap (rightAdjunct (decompose . f)) $ decompose m

instance Adjunction f g => Comonad (ACompF f g) where
        extend f = compose . fmap (leftAdjunct (f . compose)) . decompose

instance Zap ((->)e) ((,)e) where
	zapWith = zapWithGF

instance Representable ((->)e) (e,()) where
	rep = repAdjunction
	unrep = unrepAdjunction

instance Representable ((->)e) e where
	rep = id
	unrep = id

instance Adjunction ((,)e) ((->)e) where
	leftAdjunct f a e  = f (e,a)
	rightAdjunct f ~(e,a) = f a e
	unit a e = (e,a)
	counit (x,f) = f x

instance Representable Identity (Identity ()) where
	rep = repAdjunction
	unrep = unrepAdjunction

instance Adjunction Identity Identity where
	unit = Identity . Identity
	counit = runIdentity . runIdentity 

instance Zap (Reader e) (Coreader e) where
	zapWith = zapWithGF

instance Representable (Reader e) (Coreader e ()) where
	rep = repAdjunction
	unrep = unrepAdjunction

instance Adjunction (Coreader e) (Reader e) where
	unit a = Reader (\e -> Coreader e a)
	counit (Coreader x f) = runReader f x

instance ComonadContext e ((,)e `ACompF` (->)e) where
	getC = fst . decompose
	modifyC f = uncurry (flip id . f) . decompose

instance MonadState e ((->)e `ACompF` (,)e) where
	get = compose $ \s -> (s,s)
	put s = compose $ const (s,())
class ContraFunctor f => Corepresentable f x where
	corep :: (a -> x) -> f a 
	uncorep :: f a -> (a -> x)

class Functor f => Representable f x where
	rep :: (x -> a) -> f a
	unrep :: f a -> (x -> a)

{-# RULES
"rep/unrep" rep . unrep = id
"unrep/rep" unrep . rep = id
"corep/uncorep" corep . uncorep = id
"uncorep/corep" unrep . corep = id
 #-}

--repAdjunction :: Adjunction f g => (f () -> a) -> g a
--repAdjunction f = leftAdjunct f ()

--unrepAdjunction :: Adjunction f g => g a -> (f () -> a)
--unrepAdjunction = rightAdjunction . const

data EitherF a b c = EitherF (a -> c) (b -> c)

instance Functor (EitherF a b) where
        fmap f (EitherF l r) = EitherF (f . l) (f . r)

instance Representable (EitherF a b) (Either a b) where
        rep f = EitherF (f . Left) (f . Right)
        unrep (EitherF l r) = either l r

instance Representable Identity () where
	rep f = Identity (f ())
	unrep (Identity a) = const a

data Both a = Both a a 

instance Functor Both where
	fmap f (Both a b) = Both (f a) (f b)

instance Representable Both Bool where
	rep f = Both (f False) (f True)
	unrep (Both x _) False = x
	unrep (Both _ y) True = y

-- instance Adjunction f g => Representable g (f ()) where
-- instance Representable (Cofree Identity) (Free Identity ()) where


{- | Minimum definition: zapWith -}

-- zapWith :: Adjunction f g => (a -> b -> c) -> f a -> g b -> c
-- zapWith f a b = uncurry (flip f) . counit . fmap (uncurry (flip strength)) $ strength a b

-- zap :: Adjunction f g => f (a -> b) -> g a -> b
-- zap = zapWith id

class Zap f g | f -> g, g -> f where
	zapWith :: (a -> b -> c) -> f a -> g b -> c
	zap :: f (a -> b) -> g a -> b
	zap = zapWith id

(>$<) :: Zap f g => f (a -> b) -> g a -> b
(>$<) = zap

instance Zap Identity Identity where
	zapWith f (Identity a) (Identity b) = f a b

{- | Minimum definition: bizapWith -}

class Bizap p q | p -> q, q -> p where
	bizapWith :: (a -> c -> e) -> (b -> d -> e) -> p a b -> q c d -> e

	bizap :: p (a -> c) (b -> c) -> q a b -> c
	bizap = bizapWith id id

(>>$<<) :: Bizap p q => p (a -> c) (b -> c) -> q a b -> c
(>>$<<) = bizap

instance Bizap (,) Either where
	bizapWith l _ (f,_) (Left a) = l f a
	bizapWith _ r (_,g) (Right b) = r g b 

instance Bizap Either (,) where
	bizapWith l _ (Left f) (a,_) = l f a
	bizapWith _ r (Right g) (_,b) = r g b

instance (Bizap p q, Zap f g, Zap i j) => Bizap (Biff p f i) (Biff q g j) where
	bizapWith l r fs as = bizapWith (zapWith l) (zapWith r) (runBiff fs) (runBiff as)
