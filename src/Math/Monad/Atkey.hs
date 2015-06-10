{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Math.Monad.Atkey 
  ( At(..)
  , Atkey
  , Coat(..)
  , Coatkey
  ) where

data At a i j where
  At :: a -> At a i i

type Atkey m i j a = m (At a j) i

newtype Coat a i j = Coat { runCoat :: (i ~ j) => a }

type Coatkey m i j a = m (Coat a j) i
