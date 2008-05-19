--------------------------------------------------------------------
-- |
-- Module    : Control.Comonad.Memo
-- Copyright : (c) Edward Kmett 2008
-- License   : BSD3
--
-- Maintainer: Edward Kmett <ekmett@gmail.com>
-- Stability : provisional
-- Portability: portable
--
--------------------------------------------------------------------

module Control.Comonad.Memo (newMemo, Memo, pureMemo) where

import Control.Comonad
import Data.Map
import Control.Concurrent.MVar
import System.IO.Unsafe(unsafePerformIO)

-- TODO: rewrite with a memoizing context comonad
newtype Memo k v = Memo (MVar (Map k v)) (k -> v) k

newMemo :: Ord k => (k -> v) -> k -> IO (Memo k v)
newMemo f k = fmap (gen f) (newMVar Map.empty)
  where gen _ r = Memo (unsafePerformIO . update f r) ()
        update :: MVar (Data.Map k v) -> IO v
        update f r k = do 
		m <- takeMVar r
		let v = f k
                putMVar r (Map.insert k v m)
                return v

pureMemo :: (k -> v) -> Memo k v ()
pureMemo f = Memo f ()

instance Functor (Memo k v) where
	fmap f (g,a) = (g, f a)

-- TODO: map this over the memo-table!
instance Bifunctor (Memo k) where
	bimap f g (Memo h a)) = Memo (f . h) (g a)

instance Ord k => Copointed  (Memo k v) where
	extract (Memo _ a) = a
	
instance Ord k => Comonad (Memo k v) where
	duplicate (Memo f a) = Memo f (Memo f a)

