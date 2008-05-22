{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Functor.HigherOrder
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
--
-- Neil Ghani and Particia Johann''s higher order functors from
-- <http://crab.rutgers.edu/~pjohann/tlca07-rev.pdf>
----------------------------------------------------------------------------
module Control.Functor.HigherOrder 
	( HFunctor(..)
	, HPointed(..)
	, HCopointed(..)
	, HAlgebra
	, HCoalgebra
	, FixH(..)
	, LowerH(..)
	) where

import Control.Functor
import Control.Functor.Pointed
import Control.Functor.Extras
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Control.Monad.List

type HAlgebra f g = f g :~> g
type HCoalgebra f g = g :~> f g

class HFunctor f where
	ffmap :: Functor g => (a -> b) -> f g a -> f g b
	hfmap :: (g :~> h) -> f g :~> f h

newtype FixH f a = InH { outH :: f (FixH f) a }

class HFunctor m => HPointed m where
	hreturn  :: Functor f => f a -> m f a

class HFunctor w => HCopointed w where
	hextract :: Functor f => w f a -> f a

newtype LowerH 
	(h :: (* -> *) -> * -> *)
	(f :: * -> *)
	(a :: *) = LowerH { liftH :: h f a }

instance (HFunctor h, Functor f) => Functor (LowerH h f) where
	fmap f = LowerH . ffmap f . liftH 

instance (HPointed h, Pointed f) => Pointed (LowerH h f) where
	point = LowerH . hreturn . point

instance (HCopointed h, Copointed f) => Copointed (LowerH h f) where
	extract = extract . hextract . liftH

instance HFunctor (ReaderT e) where
	ffmap f g = ReaderT (fmap f . runReaderT g) 
	hfmap f g = ReaderT (f . runReaderT g)

instance HPointed (ReaderT e) where
	hreturn = ReaderT . const

instance HFunctor (StateT e) where
	ffmap f (StateT g) = StateT (fmap (first f) . g)
	hfmap f (StateT g) = StateT (f . g)

instance HPointed (StateT e) where
	hreturn m = StateT (\s -> fmap (\a -> (a,s)) m) 

instance HFunctor (WriterT e) where
	ffmap f = WriterT . fmap (first f) . runWriterT 
	hfmap f = WriterT . f . runWriterT

instance Monoid e => HPointed (WriterT e) where
	hreturn = WriterT . fmap (\a -> (a,mempty))

instance HFunctor ListT where
	ffmap f = ListT . fmap (fmap f) . runListT 
	hfmap f = ListT . f . runListT

instance HPointed ListT where
	hreturn = ListT . fmap return

{-# RULES
"hextract/hreturn" hextract . hreturn = id
 #-}
