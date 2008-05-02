-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Bifunctor.Braided
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-------------------------------------------------------------------------------------------
module Control.Bifunctor.Braided where

import Control.Bifunctor
import Control.Bifunctor.Associative

{- | A braided (co)(monoidal or associative) category can commute the arguments of its bi-endofunctor. Obeys the laws:

> idr . braid = idl 
> idl . braid = idr 
> braid . coidr = coidl 
> braid . coidl = coidr 
> associate . braid . associate = second braid . associate . first braid 
> coassociate . braid . coassociate = first braid . coassociate . second braid 

-}

class Bifunctor p => Braided p where
	braid :: p a b -> p b a

{- |
If we have a symmetric (co)'Monoidal' category, you get the additional law:

> swap . swap = id
 -}
class Braided p => Symmetric p

swap :: Symmetric p => p a b -> p b a
swap = braid

{-# RULES
"swap/swap" swap . swap = id
"braid/associate/braid"         bimap id braid . associate . bimap braid id = associate . braid . associate
"braid/coassociate/braid"       bimap braid id . coassociate . bimap id braid = coassociate . braid . coassociate
 #-}
