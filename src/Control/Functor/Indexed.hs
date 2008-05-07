-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Functor.Indexed
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
----------------------------------------------------------------------------
module Control.Functor.Indexed 
	( IxFunctor(..)
	, IxCopointed(..)
	, IxPointed(..)
	, IxApplicative(..)
	) where

class IxFunctor f where
	imap :: (a -> b) -> f j k a -> f j k b

class IxPointed m => IxApplicative m where
	iap :: m i j (a -> b) -> m j k a -> m i k b

class IxFunctor m => IxPointed m where
        ireturn :: a -> m i i a

class IxFunctor w => IxCopointed w where
	iextract :: w i i a -> a

{-# RULES
"iextract/ireturn" iextract . ireturn = id
 #-}
