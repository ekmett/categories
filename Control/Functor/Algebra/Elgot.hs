-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Functor.Algebra.Elgot
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
--
-- Elgot algebras, and their obvious dual, based on:
-- <http://www.iti.cs.tu-bs.de/~milius/research/elgot_lmcs.pdf>
--
-- Elgot algebras given you a shortcircuitable hylomorphism where you
-- can directly return a sub-answer to the catamorphism.
-- 
-- Elgot coalgebras are defined in:
-- <http://comonad.com/reader/2008/elgot-coalgebras/>
----------------------------------------------------------------------------
module Control.Functor.Algebra.Elgot
	( elgot
	, coelgot
	) where

import Control.Arrow ((|||),(&&&))
import Control.Functor.Algebra

-- | Elgot algebra
elgot :: Functor f => Algebra f a -> (b -> Either a (f b)) -> b -> a
elgot phi psi = h where h = (id ||| phi . fmap h) . psi

-- | Elgot coalgebra
coelgot :: Functor f => ((a, f b) -> b) -> Coalgebra f a -> a -> b
coelgot phi psi = h where h = phi . (id &&& fmap h . psi)
