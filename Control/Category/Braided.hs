{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	 : Control.Category.Braided
-- Copyright : 2008 Edward Kmett
-- License	 : BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-------------------------------------------------------------------------------------------
module Control.Category.Braided 
	( Braided(..)
	, Symmetric
	, swap
	) where

import Control.Categorical.Bifunctor
import Control.Category.Associative

{- | A braided (co)(monoidal or associative) category can commute the arguments of its bi-endofunctor. Obeys the laws:

> idr . braid = idl 
> idl . braid = idr 
> braid . coidr = coidl 
> braid . coidl = coidr 
> associate . braid . associate = second braid . associate . first braid 
> disassociate . braid . disassociate = first braid . disassociate . second braid 

-}

class Braided k p where
	braid :: k (p a b) (p b a)

instance Braided (->) Either where
        braid (Left a) = Right a
        braid (Right b) = Left b

instance Braided (->) (,) where
        braid ~(a,b) = (b,a)

{-# RULES
"braid/associate/braid"         second braid . associate . first braid    = associate . braid . associate
"braid/disassociate/braid"      first braid . disassociate . second braid = disassociate . braid . disassociate
  #-}

{- |
If we have a symmetric (co)'Monoidal' category, you get the additional law:

> swap . swap = id
 -}
class Braided k p => Symmetric k p

swap :: Symmetric k p => k (p a b) (p b a)
swap = braid

{-# RULES
"swap/swap" swap . swap = id
  #-}


instance Symmetric (->) Either 

instance Symmetric (->) (,)
