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
module Control.Monad.HigherOrder 
	( HFunctor(..)
	, HPointed(..)
	, HMonad(..)
	, hjoin
	, (>>**=), (=**<<)
	) where

import Control.Functor.Extras 
import Control.Functor.HigherOrder

infixl 1 >>**=
infixr 1 =**<<

class HPointed m => HMonad m where
	hbind   :: (Functor f, Functor g) => (f :~> m g) -> m f :~> m g

hjoin :: (HMonad m, Functor (m g), Functor g) => m (m g) :~> m g
hjoin = hbind id

(>>**=) :: (HMonad m, Functor f, Functor g) => m f a -> (f :~> m g) -> m g a
m >>**= k = hbind k m 

(=**<<) :: (HMonad m, Functor f, Functor g) => (f :~> m g) -> m f :~> m g
(=**<<) = hbind
