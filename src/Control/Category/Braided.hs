{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------------------
-- |
-- Module     : Control.Category.Braided
-- Copyright  : 2008-2012 Edward Kmett
-- License    : BSD
--
-- Maintainer : Edward Kmett <ekmett@gmail.com>
-- Stability  : experimental
-- Portability: portable
--
-------------------------------------------------------------------------------------------
module Control.Category.Braided
  ( Braided(..)
  , Symmetric
  , swap
  ) where

-- import Control.Categorical.Bifunctor
import Control.Category.Associative

{- | A braided (co)(monoidal or associative) category can commute the arguments of its bi-endofunctor. Obeys the laws:

> associate . braid . associate = second braid . associate . first braid
> disassociate . braid . disassociate = first braid . disassociate . second braid

If the category is Monoidal the following laws should be satisfied

> idr . braid = idl
> idl . braid = idr

If the category is Comonoidal the following laws should be satisfied

> braid . coidr = coidl
> braid . coidl = coidr

-}

class Associative k p => Braided k p where
    braid :: k (p a b) (p b a)

instance Braided (->) Either where
    braid (Left a) = Right a
    braid (Right b) = Left b

instance Braided (->) (,) where
    braid ~(a,b) = (b,a)

{-- RULES
"braid/associate/braid"         second braid . associate . first braid    = associate . braid . associate
"braid/disassociate/braid"      first braid . disassociate . second braid = disassociate . braid . disassociate
  --}

{- |
If we have a symmetric (co)'Monoidal' category, you get the additional law:

> swap . swap = id
 -}
class Braided k p => Symmetric k p

swap :: Symmetric k p => k (p a b) (p b a)
swap = braid

{-- RULES
"swap/swap" swap . swap = id
  --}

instance Symmetric (->) Either
instance Symmetric (->) (,)
