-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.HigherOrder
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
--
----------------------------------------------------------------------------
module Control.Monad.HigherOrder where

import Control.Functor.Extras
import Control.Functor.HigherOrder

class HFunctor m => HMonad m where
	hreturn :: Functor f => Natural f (m f)
	hbind   :: (Functor f, Functor g) => Natural f (m g) -> Natural (m f) (m g)
	--hbind k = hjoin . hfmap k

hjoin :: (HMonad m, Functor (m g), Functor g) => Natural (m (m g)) (m g)
hjoin = hbind id

(>>**=) :: (HMonad m, Functor f, Functor g) => m f a -> Natural f (m g) -> m g a
m >>**= k = hbind k m 
