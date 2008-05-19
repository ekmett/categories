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
----------------------------------------------------------------------------
module Control.Functor.Algebra.Elgot
	( elgot
	, coelgot
--	, g_elgot
--	, g_coelgot
	) where

import Control.Arrow ((|||),(&&&))
import Control.Functor.Algebra
-- import Control.Functor.Extras
-- import Control.Comonad
-- import Control.Monad

elgot :: Functor f => Algebra f a -> (b -> Either a (f b)) -> b -> a
elgot phi psi = h where h = (id ||| phi . fmap h) . psi

coelgot :: Functor f => ((a, f b) -> b) -> Coalgebra f a -> a -> b
coelgot phi psi = h where h = phi . (id &&& fmap h . psi)

-- ideally this would have b -> Either a (f b)
-- g_elgot :: (Comonad w, Functor f) => Dist f w -> GAlgebra f w a -> (b -> Either (w a) (f b)) -> b -> a
-- g_elgot k phi psi = extract . h where 
-- 	h = (id ||| liftW phi . k . fmap (duplicate . h)) . psi

-- g_coelgot :: (Monad m, Functor f) => Dist m f -> ((m a, f b) -> b) -> GCoalgebra f m a -> a -> b
-- g_coelgot k phi psi = h . return where
--	h = phi . (id &&& fmap (h . join) . k . liftM psi)
