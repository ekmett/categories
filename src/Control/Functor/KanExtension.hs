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
-- Some reference for the Ran monad/Lan comonad below would be nice, as I 
-- constructed them from first principles, but haven't seen them in 
-- literature.
----------------------------------------------------------------------------
module Control.Functor.KanExtension 
	( Ran(..)
	, toRan, fromRan
	, Lan(..)
	, toLan, fromLan
	) where

import Control.Functor.Composition
import Control.Functor.Extras
import Control.Functor.Pointed ()
import Control.Functor.HigherOrder
import Control.Comonad
import Control.Monad.Cont

-- | Right Kan Extension
newtype Ran g h a = Ran { runRan :: forall b. (a -> g b) -> h b }

toRan :: (Composition c, Functor k) => Natural (c k g) h -> Natural k (Ran g h)
toRan s t = Ran (s . compose . flip fmap t)

fromRan :: Composition c => Natural k (Ran g h) -> Natural (c k g) h
fromRan s = flip runRan id . s . decompose

instance HFunctor (Ran g) where
	hfmap f (Ran m) = Ran (f . m)
	ffmap f m = Ran (\k -> runRan m (k . f))

instance Functor (Ran g h) where
	fmap f m = Ran (\k -> runRan m (k . f))

instance Pointed (Ran f f) where
	point x = Ran (\k -> k x)

instance Monad (Ran f f) where
	return = point
	m >>= k = Ran (\c -> runRan m (\a -> runRan (k a) c))

-- | Left Kan Extension
data Lan g h a = forall b. Lan (g b -> a) (h b)

toLan :: (Composition c, Functor f) => Natural h (c f g) -> Natural (Lan g h) f
toLan s (Lan f v) = fmap f . decompose $ s v

fromLan :: Composition c => Natural (Lan g h) f -> Natural h (c f g)
fromLan s = compose . s . Lan id

instance Functor g => HFunctor (Lan g) where
	ffmap f (Lan g h) = Lan (f . g) h
	hfmap f (Lan g h) = Lan g (f h)

instance Functor (Lan f g) where
	fmap f (Lan g h) = Lan (f . g) h 

instance Copointed (Lan f f) where
	extract (Lan f a) = f a

instance Comonad (Lan f f) where
	duplicate (Lan f ws) = Lan (Lan f) ws
