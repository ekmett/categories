{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}

module Math.Monad.Atkey 
  ( At(..)
  , Atkey
  , at
  , Coat(..)
  , Coatkey
  , coat
  ) where

data At a i j where
  At :: a -> At a i i

at :: (a -> b) -> At a i j -> At b i j
at f (At a) = At (f a)

type Atkey m i j a = m (At a j) i

newtype Coat a i j = Coat { runCoat :: (i ~ j) => a }

type Coatkey m i j a = m (Coat a j) i

coat :: (a -> b) -> Coat a i j -> Coat b i j
coat f g = Coat (f (runCoat g))
