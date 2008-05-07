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
----------------------------------------------------------------------------
module Control.Functor.KanExtension where

import Control.Functor.Composition
import Control.Functor.Extras

-- Right Kan Extension
newtype Ran g h a = Ran { runRan :: forall b. (a -> g b) -> h b }

toRan :: (Composition c, Functor k) => Natural (c k g) h -> Natural k (Ran g h)
toRan s t = Ran (s . compose . flip fmap t)

fromRan :: Composition c => Natural k (Ran g h) -> Natural (c k g) h
fromRan s = flip runRan id . s . decompose

-- Left Kan Extension
data Lan g h a = forall b. Lan (g b -> a) (h b)

toLan :: (Composition c, Functor f) => Natural h (c f g) -> Natural (Lan g h) f
toLan s (Lan f v) = fmap f . decompose $ s v

fromLan :: Composition c => Natural (Lan g h) f -> Natural h (c f g)
fromLan s t = compose . s $ Lan id t
