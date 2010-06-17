-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Categorical
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD3
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (functional-dependencies)
--
-- A more categorical definition of Functor than endofunctors in the category Hask
-------------------------------------------------------------------------------------------
module Control.Functor.Categorical
	( CFunctor (cmap)
	, CPointed (creturn)
	, CCopointed (cextract)
	, CBind (cbind, cjoin)
	, CExtend (cextend, cduplicate)
	, CDistributes (cdist)
	, CMonad
	, CComonad
	) where

import Prelude hiding (id,(.))
import Control.Category
import Control.Category.Hask
import Control.Monad.Identity
import Control.Monad.List
import Control.Monad.Cont

import Control.Monad.Reader
import Control.Monad.Writer as LW
import Control.Monad.State as LS

import Control.Monad.RWS as LRWS
import Control.Monad.Writer.Strict as SW
import Control.Monad.State.Strict as SS
import Control.Monad.RWS.Strict as SRWS

class (Category r, Category s) => CFunctor f r s | f r -> s,  f s -> r where
	cmap :: r a b -> s (f a) (f b)

instance CFunctor ([]) Hask Hask where cmap = fmap 
instance CFunctor Maybe Hask Hask where cmap = fmap
instance CFunctor (Either a) Hask Hask where cmap = fmap 
instance CFunctor Identity Hask Hask where cmap = fmap
instance CFunctor ((,)e) Hask Hask where cmap = fmap
instance CFunctor (Reader e) Hask Hask where cmap = fmap
instance CFunctor (LW.Writer e) Hask Hask where cmap = fmap
instance CFunctor (LS.State s) Hask Hask where cmap = fmap
instance CFunctor (Cont e) Hask Hask where cmap = fmap
instance CFunctor (LRWS.RWS r w s) Hask Hask where cmap = fmap
instance CFunctor IO Hask Hask where cmap = fmap

instance Monad m => CFunctor (ReaderT e m) Hask Hask where cmap = fmap
instance Monad m => CFunctor (LW.WriterT e m) Hask Hask where cmap = fmap
instance Monad m => CFunctor (LS.StateT e m) Hask Hask where cmap = fmap
instance Monad m => CFunctor (ContT r m) Hask Hask where cmap = fmap
instance Monad m => CFunctor (ListT m) Hask Hask where cmap = fmap
instance Monad m => CFunctor (LRWS.RWST r w s m) Hask Hask where cmap = fmap

#if __GLASGOW_HASKELL__ >= 608
instance CFunctor (SW.Writer e) Hask Hask where cmap = fmap
instance CFunctor (SS.State s) Hask Hask where cmap = fmap
instance CFunctor (SRWS.RWS r w s) Hask Hask where cmap = fmap
instance Monad m => CFunctor (SW.WriterT w m) Hask Hask where cmap = fmap
instance Monad m => CFunctor (SS.StateT s m) Hask Hask where cmap = fmap
instance Monad m => CFunctor (SRWS.RWST r w s m) Hask Hask where cmap = fmap
#endif

class CFunctor m (~>) (~>) => CBind m (~>) where
        cjoin :: m (m a) ~> m a
        cbind :: (a ~> m b) -> (m a ~> m b)

        cjoin = cbind id
        cbind f = cjoin . cmap f

class CFunctor w (~>) (~>) => CExtend w (~>) where
        cduplicate :: w a ~> w (w a)
        cextend :: (w a ~> b) -> (w a ~> w b)

        cduplicate = cextend id
        cextend f = cmap f . cduplicate

class CFunctor m (~>) (~>) => CPointed m (~>) where
	creturn :: a ~> m a

class CFunctor w (~>) (~>) => CCopointed w (~>) where
	cextract :: w a ~> a

class (CFunctor f (~>) (~>), CFunctor g (~>) (~>)) => CDistributes f g (~>) where
	cdist :: f (g a) ~> g (f a)

class (CPointed m (~>), CBind m (~>)) => CMonad m (~>) 
instance (CPointed m (~>), CBind m (~>)) => CMonad m (~>) 

class (CCopointed m (~>), CExtend m (~>)) => CComonad m (~>) 
instance (CCopointed m (~>), CExtend m (~>)) => CComonad m (~>) 
