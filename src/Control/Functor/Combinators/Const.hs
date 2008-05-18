-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Combinators.Const
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD3
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-------------------------------------------------------------------------------------------

module Control.Functor.Combinators.Const
	( Const2(Const2,runConst2)
	) where

import Data.Monoid
import Control.Applicative
import Control.Applicative.Parameterized ()
import Control.Monad
import Control.Category.Hask
import Control.Category.Associative
import Control.Category.Braided
import Control.Functor
import Control.Functor.Exponential
import Control.Functor.Contra
import Control.Functor.Zip
import Control.Functor.Pointed
import Control.Monad.Parameterized
import Control.Comonad.Parameterized ()

newtype Const2 t a b = Const2 { runConst2 :: t } 

instance QFunctor (Const2 t) Hask Hask where
	second _ = Const2 . runConst2

instance PFunctor (Const2 t) Hask Hask where
	first _ = Const2 . runConst2

instance Bifunctor (Const2 t) Hask Hask Hask where
	bimap _ _ = Const2 . runConst2

instance Associative (Const2 t) Hask where
	associate = Const2 . runConst2

instance Coassociative (Const2 t) Hask where
	coassociate = Const2 . runConst2

instance Braided (Const2 t) Hask where
	braid = Const2 . runConst2

instance Symmetric (Const2 t) Hask

instance Monoid t => Zip (Const2 t a) where
	fzipWith _ a b = Const2 (runConst2 a `mappend` runConst2 b)

instance Monoid t => Bizip (Const2 t) where
	bizipWith _ _ a b = Const2 (runConst2 a `mappend` runConst2 b)

instance Functor (Const2 t a) where
	fmap _ = Const2 . runConst2

instance ContraFunctor (Const2 t a) where
	contramap _ = Const2 . runConst2

instance ExpFunctor (Const2 t a) where
	xmap _ _ = Const2 . runConst2

instance Monoid t => Pointed (Const2 t a) where
	point _ = Const2 mempty

instance Monoid t => PPointed (Const2 t) where
	preturn _ = Const2 mempty

instance Monoid t => Applicative (Const2 t a) where
	pure _ = Const2 mempty
	f <*> a = Const2 (runConst2 f `mappend` runConst2 a)

instance Monoid t => PApplicative (Const2 t) where
	pap f a = Const2 (runConst2 f `mappend` runConst2 a)

instance Monoid t => Monad (Const2 t a) where
	return _ = Const2 mempty
	m >>= _ = Const2 $ runConst2 m 

instance Monoid t => PMonad (Const2 t) where
	pbind _ = Const2 . runConst2

instance Monoid t => Monoid (Const2 t a b) where
	mempty = Const2 mempty
	mappend a b = Const2 (runConst2 a `mappend` runConst2 b)

instance Monoid t => MonadPlus (Const2 t a) where
	mzero = Const2 mempty
	mplus a b = Const2 (runConst2 a `mappend` runConst2 b)
