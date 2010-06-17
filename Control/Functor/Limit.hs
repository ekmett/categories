-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Functor.Limit
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism/existentials)
--
----------------------------------------------------------------------------
module Control.Functor.Limit
	( Limit, HasLimit(limit)
	, Colimit(..)
	, liftLimit, liftColimit
	) where

import Prelude hiding (abs)
import Control.Functor.Extras
import Data.Monoid

-- | @type Limit = Ran (Const Void)@
-- Limit { runLimit :: forall a. f a }
type Limit f = forall a. f a 

class HasLimit f where
	limit :: f a

instance HasLimit Maybe where
	limit = Nothing

instance HasLimit [] where
	limit = []

instance Monoid a => HasLimit (Either a) where
	limit = (Left mempty)

liftLimit :: (f :~> g) -> Limit f -> Limit g
liftLimit f a = f a

-- | @type Colimit = Lan (Const Void)@
data Colimit f = forall b. Colimit (f b)

liftColimit :: (f :~> g) -> Colimit f -> Colimit g
liftColimit f (Colimit a) = Colimit (f a)
