-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Functor.Limit
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism/existentials in Ran/Lan)
--
----------------------------------------------------------------------------
module Control.Functor.Limit
	( Lim
	, Colim
	, module Control.Functor.KanExtension
	, Void
	, Const
	) where

import Prelude hiding (abs)
import Control.Applicative (Const)
import Data.Void (Void)
import Control.Functor.KanExtension

type Lim = Ran (Const Void)
type Colim = Lan (Const Void)
