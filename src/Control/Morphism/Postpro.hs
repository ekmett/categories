{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Morphism.Postpro
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
-- 
-- See Maarten Fokkinga''s PhD Dissertation for postpro. g_postpro is 
-- an obvious generalization.
----------------------------------------------------------------------------
module Control.Morphism.Postpro 
	( postpro
	, g_postpro
	) where

import Control.Monad
import Control.Functor.Algebra 
import Control.Functor.Extras
import Control.Functor.Fix
import Control.Morphism.Ana

-- prepro f e = x where x = f . fmap (x . cata (InF . e)) . outF
postpro :: Functor f => (c -> f c) -> (f :~> f) -> c -> FixF f
postpro g e = x where x = InF . fmap (ana (e . outF) . x) . g

-- | Generalized postpromorphisms
g_postpro :: (Functor f, Monad m) => Dist m f -> GCoalgebra f m a -> (f :~> f) -> a -> FixF f
g_postpro k g e = a . return where a = InF . fmap (ana (e . outF) . a . join) . k . liftM g
