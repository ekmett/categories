{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, TypeOperators, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
-------------------------------------------------------------------------------------------
-- |
-- Module    : Control.Category.Cartesian
-- Copyright : 2008-2010 Edward Kmett
-- License   : BSD
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (class-associated types)
--
-------------------------------------------------------------------------------------------
module Control.Category.Cartesian
    (
    -- * (Co)Cartesian categories
      Cartesian(..)
    , bimapProduct, braidProduct, associateProduct, disassociateProduct
    , CoCartesian(..)
    , bimapSum, braidSum, associateSum, disassociateSum
    ) where

import Control.Category.Braided
import Control.Category.Monoidal
import Prelude hiding (Functor, map, (.), id, fst, snd, curry, uncurry)
import qualified Prelude (fst,snd)
import Control.Categorical.Bifunctor
import Control.Category

infixr 3 &&&
infixr 2 |||

{- |
Minimum definition:

> fst, snd, diag
> fst, snd, (&&&)
-}
class (Symmetric k (Product k), Monoidal k (Product k)) => Cartesian k where
    type Product k :: * -> * -> *
    fst :: Product k a b `k` a
    snd :: Product k a b `k` b
    diag :: a `k` Product k a a
    (&&&) :: (a `k` b) -> (a `k` c) -> a `k` Product k b c

    diag = id &&& id
    f &&& g = bimap f g . diag

{-- RULES
"fst . diag"      fst . diag = id
"snd . diag"    snd . diag = id
"fst . f &&& g" forall f g. fst . (f &&& g) = f
"snd . f &&& g" forall f g. snd . (f &&& g) = g
 --}

instance Cartesian (->) where
    type Product (->) = (,)
    fst = Prelude.fst
    snd = Prelude.snd
    diag a = (a,a)
    (f &&& g) a = (f a, g a)

-- | free construction of 'Bifunctor' for the product 'Bifunctor' @Product k@ if @(&&&)@ is known
bimapProduct :: Cartesian k => k a c -> k b d -> Product k a b `k` Product k c d
bimapProduct f g = (f . fst) &&& (g . snd)

-- | free construction of 'Braided' for the product 'Bifunctor' @Product k@
braidProduct :: Cartesian k => k (Product k a b) (Product k b a)
braidProduct = snd &&& fst

-- | free construction of 'Associative' for the product 'Bifunctor' @Product k@
associateProduct :: Cartesian k => Product k (Product k a b) c `k` Product k a (Product k b c)
associateProduct = (fst . fst) &&& first snd

-- | free construction of 'Disassociative' for the product 'Bifunctor' @Product k@
disassociateProduct:: Cartesian k => Product k a (Product k b c) `k` Product k (Product k a b) c
disassociateProduct= braid . second braid . associateProduct . first braid . braid

-- * Co-Cartesian categories

-- a category that has finite coproducts, weakened the same way as PreCartesian above was weakened
class (Monoidal k (Sum k), Symmetric k (Sum k)) => CoCartesian k where
    type Sum k :: * -> * -> *
    inl :: a `k` Sum k a b
    inr :: b `k` Sum k a b
    codiag :: Sum k a a `k` a
    (|||) :: k a c -> k b c -> Sum k a b `k` c

    codiag = id ||| id
    f ||| g = codiag . bimap f g

{-- RULES
"codiag . inl"  codiag . inl = id
"codiag . inr"    codiag . inr = id
"(f ||| g) . inl" forall f g. (f ||| g) . inl = f
"(f ||| g) . inr" forall f g. (f ||| g) . inr = g
 --}

instance CoCartesian (->) where
    type Sum (->) = Either
    inl = Left
    inr = Right
    codiag (Left a) = a
    codiag (Right a) = a
    (f ||| _) (Left a) = f a
    (_ ||| g) (Right a) = g a

-- | free construction of 'Bifunctor' for the coproduct 'Bifunctor' @Sum k@ if @(|||)@ is known
bimapSum :: CoCartesian k => k a c -> k b d -> Sum k a b `k` Sum k c d
bimapSum f g = (inl . f) ||| (inr . g)

-- | free construction of 'Braided' for the coproduct 'Bifunctor' @Sum k@
braidSum :: CoCartesian k => Sum k a b `k` Sum k b a
braidSum = inr ||| inl

-- | free construction of 'Associative' for the coproduct 'Bifunctor' @Sum k@
associateSum :: CoCartesian k => Sum k (Sum k a b) c `k` Sum k a (Sum k b c)
associateSum = braid . first braid . disassociateSum . second braid . braid

-- | free construction of 'Disassociative' for the coproduct 'Bifunctor' @Sum k@
disassociateSum :: CoCartesian k => Sum k a (Sum k b c) `k` Sum k (Sum k a b) c
disassociateSum = (inl . inl) ||| first inr
