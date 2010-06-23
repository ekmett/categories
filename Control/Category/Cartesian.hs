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
    -- * Pre-(Co)Cartesian categories
      PreCartesian(..)
    , bimapProduct, braidProduct, associateProduct, disassociateProduct
    , PreCoCartesian(..)
    , bimapSum, braidSum, associateSum, disassociateSum
    -- * (Co)Cartesian categories
    , Cartesian
    , CoCartesian
    ) where

import Control.Category.Associative
import Control.Category.Braided
import Control.Category.Monoidal
import Prelude hiding (Functor, map, (.), id, fst, snd, curry, uncurry)
import qualified Prelude (fst,snd)
import Control.Categorical.Bifunctor
import Control.Category

infixr 3 &&&
infixr 2 |||

{- |
NB: This is weaker than traditional category with products! That is Cartesian, below.
The problem is @(->)@ lacks an initial object, since every type is inhabited in Haskell.
Consequently its coproduct is merely a semigroup, not a monoid (as it has no identity), and 
since we want to be able to describe its dual category, which has this non-traditional 
form being built over a category with an associative bifunctor rather than as a monoidal category
for the product monoid.

Minimum definition: 

> fst, snd, diag 
> fst, snd, (&&&)
-}
class ( Associative k (Product k)
      , Disassociative k (Product k)
      , Symmetric k (Product k)
      , Braided k (Product k)
      ) => PreCartesian k where
    type Product k :: * -> * -> *
    fst :: Product k a b `k` a
    snd :: Product k a b `k` b
    diag :: a `k` Product k a a
    (&&&) :: (a `k` b) -> (a `k` c) -> a `k` Product k b c

    diag = id &&& id
    f &&& g = bimap f g . diag


{-# RULES
"fst . diag"      fst . diag = id
"snd . diag"    snd . diag = id
"fst . f &&& g" forall f g. fst . (f &&& g) = f
"snd . f &&& g" forall f g. snd . (f &&& g) = g
 #-}

instance PreCartesian (->) where
    type Product (->) = (,)
    fst = Prelude.fst
    snd = Prelude.snd
    diag a = (a,a)
    (f &&& g) a = (f a, g a)

-- alias
class ( Monoidal k (Product k)
      , PreCartesian k
      ) => Cartesian k
instance ( Monoidal k (Product k)
         , PreCartesian k
         ) => Cartesian k

-- | free construction of 'Bifunctor' for the product 'Bifunctor' @Product k@ if @(&&&)@ is known
bimapProduct :: (PreCartesian k, (<*>) ~ Product k) => (a `k` c) -> (b `k` d) -> (a <*> b) `k` (c <*> d)
bimapProduct f g = (f . fst) &&& (g . snd)
    
-- | free construction of 'Braided' for the product 'Bifunctor' @Product k@
-- braidProduct :: (PreCartesian k, Product k ~ (<*>))  => a <*> b ~> b <*> a
braidProduct :: (PreCartesian k) => Product k a b `k` Product k b a
braidProduct = snd &&& fst

-- | free construction of 'Associative' for the product 'Bifunctor' @Product k@
-- associateProduct :: (PreCartesian k, (<*>) ~ Product k)  => (a <*> b) <*> c ~> (a <*> (b <*> c))
associateProduct :: (PreCartesian k) => Product k (Product k a b) c `k` Product k a (Product k b c)
associateProduct = (fst . fst) &&& first snd

-- | free construction of 'Disassociative' for the product 'Bifunctor' @Product k@
-- disassociateProduct:: (PreCartesian k,  (<*>) ~ Product k) => a <*> (b <*> c) ~> (a <*> b) <*> c
disassociateProduct:: (PreCartesian k) => Product k a (Product k b c) `k` Product k (Product k a b) c
disassociateProduct= braid . second braid . associateProduct . first braid . braid 

-- * Co-PreCartesian categories

-- a category that has finite coproducts, weakened the same way as PreCartesian above was weakened
class ( Associative k (Sum k)
      , Disassociative k (Sum k)
      , Symmetric k (Product k)
      , Braided k (Sum k)
      ) => PreCoCartesian k where
    type Sum k :: * -> * -> *
    inl :: a `k` Sum k a b
    inr :: b `k` Sum k a b
    codiag :: Sum k a a `k` a
    (|||) :: (a `k` c) -> (b `k` c) -> Sum k a b `k` c

    codiag = id ||| id
    f ||| g = codiag . bimap f g

{-# RULES
"codiag . inl"  codiag . inl = id
"codiag . inr"    codiag . inr = id
"(f ||| g) . inl" forall f g. (f ||| g) . inl = f
"(f ||| g) . inr" forall f g. (f ||| g) . inr = g
 #-}

instance PreCoCartesian (->) where
    type Sum (->) = Either
    inl = Left
    inr = Right
    codiag (Left a) = a
    codiag (Right a) = a
    (f ||| _) (Left a) = f a 
    (_ ||| g) (Right a) = g a

-- | free construction of 'Bifunctor' for the coproduct 'Bifunctor' @Sum k@ if @(|||)@ is known
bimapSum :: (PreCoCartesian k, Sum k ~ (+)) => (a `k` c) -> (b `k` d) -> (a + b) `k` (c + d)
bimapSum f g = (inl . f) ||| (inr . g)

-- | free construction of 'Braided' for the coproduct 'Bifunctor' @Sum k@
braidSum :: (PreCoCartesian k, (+) ~ Sum k) => (a + b) `k` (b + a)
braidSum = inr ||| inl

-- | free construction of 'Associative' for the coproduct 'Bifunctor' @Sum k@
-- associateSum :: (PreCoCartesian k, (+) ~ Sum k) => ((a + b) + c) ~> (a + (b + c))
associateSum :: (PreCoCartesian k) => Sum k (Sum k a b) c `k` Sum k a (Sum k b c)
associateSum = braid . first braid . disassociateSum . second braid . braid

-- | free construction of 'Disassociative' for the coproduct 'Bifunctor' @Sum k@
-- disassociateSum :: (PreCoCartesian k, (+) ~ Sum k) => (a + (b + c)) ~> ((a + b) + c)
disassociateSum :: (PreCoCartesian k) => Sum k a (Sum k b c) `k` Sum k (Sum k a b) c
disassociateSum = (inl . inl) ||| first inr

class 
    ( Comonoidal k (Sum k)
    , PreCoCartesian k
    ) => CoCartesian k
instance 
    ( Comonoidal k (Sum k)
    , PreCoCartesian k
    ) => CoCartesian k
