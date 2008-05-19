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
----------------------------------------------------------------------------
module Control.Functor.Algebra.Elgot
	( elgot
	, coelgot
--	, g_elgot
	) where

import Control.Arrow ((|||),(&&&),(+++))
import Control.Functor.Algebra
import Control.Functor.Extras
import Control.Comonad

elgot :: Functor f => Algebra f a -> (b -> Either a (f b)) -> b -> a
elgot a e = h where h = (id ||| a . fmap h) . e 

coelgot :: Functor f => ((a, f b) -> b) -> Coalgebra f a -> a -> b
coelgot e a = h where h = e . (id &&& fmap h . a)

-- g_elgot :: (Comonad w, Functor f) => Dist f w -> GAlgebra f w a -> (b -> Either a (f b)) -> b -> a
-- g_elgot k a e = (id ||| extract) . h where 
--	h :: b -> Either a (w a)
--	h = (id +++ liftW a . k . fmap (duplicate . h)) . e 
