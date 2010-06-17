-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Functor.KanExtension
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
--
-- Left and right Kan extensions, expressed as higher order functors
--
-- See <http://comonad.com/reader/2008/kan-extensions/>
-- and <http://comonad.com/reader/2008/kan-extensions-ii/>
-- for motivation.
--
-- NB: @Yoneda@, @CoYoneda@, @Density@, @Codensity@ have been factored
-- out into separate modules.
----------------------------------------------------------------------------
module Control.Functor.KanExtension 
	( 
	-- * Right Kan Extensions
	  Ran(..)
	, toRan, fromRan
	, adjointToRan, ranToAdjoint
	, ranToComposedAdjoint, composedAdjointToRan
	, composeRan, decomposeRan
	-- * Left Kan Extensions
	, Lan(..)
	, toLan, fromLan
	, adjointToLan, lanToAdjoint
	, composeLan, decomposeLan
	, lanToComposedAdjoint, composedAdjointToLan
	) where

import Prelude hiding (abs)
import Control.Functor.Composition
import Control.Functor.Extras
import Control.Functor.Pointed ()
import Control.Functor.HigherOrder
import Control.Functor.Adjunction
import Control.Monad.Identity


-- | The right Kan Extension of h along g.
-- An alternative definition in terms of Ends.
--
-- @newtype RanT g h a b b' { (a -> g b) -> h b' }@
--
-- @type Ran g h a = End (RanT g h a)@
newtype Ran g h a = Ran { runRan :: forall b. (a -> g b) -> h b }

-- | Nat(k `o` g, h) is isomorphic to Nat(k, Ran g h) (forwards)
toRan :: (Composition o, Functor k) => (k `o` g :~> h) -> k :~> Ran g h
toRan s t = Ran (s . compose . flip fmap t)

-- | Nat(k `o` g, h) is isomorphic to Nat(k, Ran g h) (backwards)
fromRan :: Composition o => (k :~> Ran g h) -> (k `o` g) :~> h
fromRan s = flip runRan id . s . decompose

instance HFunctor (Ran g) where
	hfmap f (Ran m) = Ran (f . m)
	ffmap f m = Ran (\k -> runRan m (k . f))

instance Functor (Ran g h) where
	fmap f m = Ran (\k -> runRan m (k . f))

-- | The natural isomorphism from @Ran f (Ran g h)@ to @Ran (f `o` g) h@ (forwards)
composeRan :: Composition o => Ran f (Ran g h) :~> Ran (f `o` g) h
composeRan r = Ran (\f -> runRan (runRan r (decompose . f)) id)

-- | The natural isomorphism from @Ran f (Ran g h)@ to @Ran (f `o` g) h@ (backwards)
decomposeRan :: (Functor f, Composition o) => Ran (f `o` g) h :~> Ran f (Ran g h)
decomposeRan r = Ran (\f -> Ran (\g -> runRan r (compose . fmap g . f)))

-- | @f -| g@ iff @Ran g Identity@ exists (forward)
adjointToRan :: Adjunction f g => f :~> Ran g Identity
adjointToRan f = Ran (\a -> Identity $ rightAdjunct a f)

-- | @f -| g@ iff @Ran g Identity@ exists (backwards)
ranToAdjoint :: Adjunction f g => Ran g Identity :~> f
ranToAdjoint r = runIdentity (runRan r unit)

ranToComposedAdjoint :: (Composition o, Adjunction f g) => Ran g h :~> (h `o` f)
ranToComposedAdjoint r = compose (runRan r unit)

composedAdjointToRan :: (Functor h, Composition o, Adjunction f g) => (h `o` f) :~> Ran g h
composedAdjointToRan f = Ran (\a -> fmap (rightAdjunct a) (decompose f))

-- | Left Kan Extension
--
-- @newtype LanT g h a b b' { (g b -> a, h b') }@
--
-- @type Lan g h a = Coend (LanT g h a)@
data Lan g h a = forall b. Lan (g b -> a) (h b)

-- | @Nat(h, f.g)@ is isomorphic to @Nat (Lan g h, f)@ (forwards)
toLan :: (Composition o, Functor f) => (h :~> (f `o` g)) -> Lan g h :~> f
toLan s (Lan f v) = fmap f . decompose $ s v

-- | @Nat(h, f.g)@ is isomorphic to @Nat (Lan g h, f)@ (backwards)
fromLan :: Composition o => (Lan g h :~> f) -> h :~> (f `o` g)
fromLan s = compose . s . Lan id

instance Functor g => HFunctor (Lan g) where
	ffmap f (Lan g h) = Lan (f . g) h
	hfmap f (Lan g h) = Lan g (f h)

instance Functor (Lan f g) where
	fmap f (Lan g h) = Lan (f . g) h

-- | f -| g iff Lan f Identity is inhabited (forwards)
adjointToLan :: Adjunction f g => g :~> Lan f Identity
adjointToLan = Lan counit . Identity

-- | f -| g iff Lan f Identity is inhabited (backwards)
lanToAdjoint :: Adjunction f g => Lan f Identity :~> g
lanToAdjoint (Lan f v) = leftAdjunct f (runIdentity v)

lanToComposedAdjoint :: (Functor h, Composition o, Adjunction f g) => Lan f h :~> (h `o` g)
lanToComposedAdjoint (Lan f v) = compose (fmap (leftAdjunct f) v)

composedAdjointToLan :: (Composition o, Adjunction f g) => (h `o` g) :~> Lan f h 
composedAdjointToLan = Lan counit . decompose

-- | the natural isomorphism from @Lan f (Lan g h)@ to @Lan (f `o` g) h@ (forwards)
composeLan :: (Functor f, Composition o) => Lan f (Lan g h) :~> Lan (f `o` g) h
composeLan (Lan f (Lan g h)) = Lan (f . fmap g . decompose) h

-- | the natural isomorphism from @Lan f (Lan g h)@ to @Lan (f `o` g) h@ (backwards)
decomposeLan :: Composition o => Lan (f `o` g) h :~> Lan f (Lan g h)
decomposeLan (Lan f h) = Lan (f . compose) (Lan id h)

