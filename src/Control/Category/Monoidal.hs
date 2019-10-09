{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
-------------------------------------------------------------------------------------------
-- |
-- Module    : Control.Category.Monoidal
-- Copyright : 2008,2012 Edward Kmett
-- License   : BSD
--
-- Maintainer : Edward Kmett <ekmett@gmail.com>
-- Stability  : experimental
-- Portability: non-portable (class-associated types)
--
-- A 'Monoidal' category is a category with an associated biendofunctor that has an identity,
-- which satisfies Mac Lane''s pentagonal and triangular coherence conditions
-- Technically we usually say that category is 'Monoidal', but since
-- most interesting categories in our world have multiple candidate bifunctors that you can
-- use to enrich their structure, we choose here to think of the bifunctor as being
-- monoidal. This lets us reuse the same 'Bifunctor' over different categories without
-- painful newtype wrapping.

-------------------------------------------------------------------------------------------

module Control.Category.Monoidal
  ( Monoidal(..)
  ) where

import Control.Category.Associative
import Data.Void
import Control.Arrow (Kleisli(..))

-- | Denotes that we have some reasonable notion of 'Identity' for a particular 'Bifunctor' in this 'Category'. This
-- notion is currently used by both 'Monoidal' and 'Comonoidal'

{- | A monoidal category. 'idl' and 'idr' are traditionally denoted lambda and rho
 the triangle identities hold:

> first idr = second idl . associate
> second idl = first idr . associate
> first idr = disassociate . second idl
> second idl = disassociate . first idr
> idr . coidr = id
> idl . coidl = id
> coidl . idl = id
> coidr . idr = id

-}

class Associative k p => Monoidal (k :: * -> * -> *) (p :: * -> * -> *) where
  type Id (k :: * -> * -> *) (p :: * -> * -> *) :: *
  idl   :: k (p (Id k p) a) a
  idr   :: k (p a (Id k p)) a
  coidl :: k a (p (Id k p) a)
  coidr :: k a (p a (Id k p))

instance Monoidal (->) (,) where
  type Id (->) (,) = ()
  idl = snd
  idr = fst
  coidl a = ((),a)
  coidr a = (a,())

instance Monoidal (->) Either where
  type Id (->) Either = Void
  idl = either absurd id
  idr = either id absurd
  coidl = Right
  coidr = Left

instance Monad m => Monoidal (Kleisli m) (,) where
  type Id (Kleisli m) (,) = ()
  idl = Kleisli $ return . snd
  idr = Kleisli $ return . fst
  coidl = Kleisli $ return . (,) ()
  coidr = Kleisli $ return . flip (,) ()

instance Monad m => Monoidal (Kleisli m) Either where
  type Id (Kleisli m) Either = Void
  idl = Kleisli $ either absurd return
  idr = Kleisli $ either return absurd
  coidl = Kleisli $ return . Right
  coidr = Kleisli $ return . Left

{-- RULES
-- "bimap id idl/associate"   second idl . associate = first idr
-- "bimap idr id/associate"   first idr . associate = second idl
-- "disassociate/bimap id idl"  disassociate . second idl = first idr
-- "disassociate/bimap idr id"  disassociate . first idr = second idl
"idr/coidr" idr . coidr = id
"idl/coidl"  idl . coidl = id
"coidl/idl"  coidl . idl = id
"coidr/idr"  coidr . idr = id
"idr/braid" idr . braid = idl
"idl/braid" idl . braid = idr
"braid/coidr" braid . coidr = coidl
"braid/coidl" braid . coidl = coidr
 --}

