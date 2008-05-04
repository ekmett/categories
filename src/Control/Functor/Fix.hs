-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Functor.Fix
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
--
----------------------------------------------------------------------------
module Control.Functor.Fix where

import Control.Monad
import Control.Comonad
import Control.Functor.Algebra

newtype Fix f = InF { outF :: f (Fix f) }

outM :: (Functor f, Monad m) => CoAlgM f m (Fix f)
outM = liftCoAlg outF

inW :: (Functor f, Comonad w) => AlgW f w (Fix f)
inW = liftAlg InF

