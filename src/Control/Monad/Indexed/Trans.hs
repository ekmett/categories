-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Indexed.Trans
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental 
-- Portability :  portable (indexed monad transfomers)
--
-- TODO: figure out a meaningful way for indexed monads to transform indexed 
-- monads
----------------------------------------------------------------------------
module Control.Monad.Indexed.Trans where

class IxMonadTrans t where
	ilift :: Monad m => m a -> t m i i a 

