-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Functor.End
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
--
----------------------------------------------------------------------------
module Control.Functor.End where

newtype End f = End { runEnd :: forall i. f i i } 
data Coend f = forall i. Coend { runCoend :: f i i } 
