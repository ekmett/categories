-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD3
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (functional-dependencies)
--
-- A more categorical definition of Functor than endofunctors in the category Hask
-------------------------------------------------------------------------------------------
module Control.Functor
	( CFunctor (cmap)
	, PFunctor (first), first'
	, QFunctor (second), second'
	, Bifunctor (bimap)
	) where

import Prelude hiding (id,(.))
import Control.Category
import Control.Category.Hask
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.List
import Control.Monad.Cont
import Control.Monad.Writer.Strict as SW
import Control.Monad.Writer.Lazy as LW
import Control.Monad.State.Strict as SS
import Control.Monad.State.Lazy as LS
import Control.Monad.RWS.Strict as SRWS
import Control.Monad.RWS.Lazy as LRWS


class (Category r, Category t) => PFunctor p r t | p r -> t, p t -> r where
	first :: r a b -> t (p a c) (p b c)

{-# INLINE first' #-}
first' :: Bifunctor p r s t => r a b -> t (p a c) (p b c)
first' f = bimap f id

class (Category s, Category t) => QFunctor q s t | q s -> t, q t -> s where
	second :: s a b -> t (q c a) (q c b)

{-# INLINE second' #-}
second' :: Bifunctor p r s t => s a b -> t (p c a) (p c b)
second' = bimap id

instance PFunctor Either Hask Hask where
	first = first'

instance QFunctor Either Hask Hask where
	second = second'

instance Bifunctor Either Hask Hask Hask where
        bimap f _ (Left a) = Left (f a)
	bimap _ g (Right a) = Right (g a)

instance QFunctor (->) Hask Hask where
	second = (.)

instance PFunctor (,) Hask Hask where
	first = first'

instance QFunctor (,) Hask Hask where
	second = second'

instance Bifunctor (,) Hask Hask Hask where
        bimap f g ~(a,b)= (f a, g b)

class (PFunctor p r t, QFunctor p s t) => Bifunctor p r s t | p r -> s t, p s -> r t, p t -> r s where
	bimap :: r a b -> s c d -> t (p a c) (p b d)

class (Category r, Category s) => CFunctor f r s | f r -> s,  f s -> r where
	cmap :: r a b -> s (f a) (f b)

instance CFunctor ([]) Hask Hask where cmap = fmap 
instance CFunctor Maybe Hask Hask where cmap = fmap
instance CFunctor (Either a) Hask Hask where cmap = fmap 
instance CFunctor Identity Hask Hask where cmap = fmap
instance CFunctor ((,)e) Hask Hask where cmap = fmap
instance CFunctor (Reader e) Hask Hask where cmap = fmap
instance CFunctor (LW.Writer e) Hask Hask where cmap = fmap
instance CFunctor (SW.Writer e) Hask Hask where cmap = fmap
instance CFunctor (LS.State s) Hask Hask where cmap = fmap
instance CFunctor (SS.State s) Hask Hask where cmap = fmap
instance CFunctor (Cont e) Hask Hask where cmap = fmap
instance CFunctor (SRWS.RWS r w s) Hask Hask where cmap = fmap
instance CFunctor (LRWS.RWS r w s) Hask Hask where cmap = fmap
instance CFunctor IO Hask Hask where cmap = fmap

instance Monad m => CFunctor (ReaderT e m) Hask Hask where cmap = fmap
instance Monad m => CFunctor (LW.WriterT e m) Hask Hask where cmap = fmap
instance Monad m => CFunctor (LS.StateT e m) Hask Hask where cmap = fmap
instance Monad m => CFunctor (ContT r m) Hask Hask where cmap = fmap
instance Monad m => CFunctor (ListT m) Hask Hask where cmap = fmap
instance Monad m => CFunctor (LRWS.RWST r w s m) Hask Hask where cmap = fmap
instance Monad m => CFunctor (SW.WriterT w m) Hask Hask where cmap = fmap
instance Monad m => CFunctor (SS.StateT s m) Hask Hask where cmap = fmap
instance Monad m => CFunctor (SRWS.RWST r w s m) Hask Hask where cmap = fmap

