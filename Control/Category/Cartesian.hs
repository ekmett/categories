{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
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
    ( module Control.Category.Associative
    , module Control.Category.Monoidal
    -- * Pre-(Co)Cartesian categories
    , PreCartesian(..)
    , bimapPreCartesian, braidPreCartesian, associatePreCartesian, disassociatePreCartesian
    , PreCoCartesian(..)
    , bimapPreCoCartesian, braidPreCoCartesian, associatePreCoCartesian, disassociatePreCoCartesian
    -- * (Co)Cartesian categories
    , Cartesian
    , CoCartesian
    ) where

import Control.Category.Hask
import Control.Category.Associative
import Control.Category.Monoidal
import Prelude hiding (Functor, map, (.), id, fst, snd, curry, uncurry)
import qualified Prelude (fst,snd)
import Control.Functor
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
class (Associative (~>) (Product (~>)), Coassociative (~>) (Product (~>)), Braided (~>) (Product (~>))) => PreCartesian (~>) where
    type Product k :: * -> * -> *
    fst :: Product (~>) a b ~> a
    snd :: Product (~>) a b ~> b
    diag :: a ~> Product (~>) a a
    (&&&) :: (a ~> b) -> (a ~> c) -> a ~> Product (~>) b c

    diag = id &&& id
    f &&& g = bimap f g . diag


{-# RULES
"fst . diag"      fst . diag = id
"snd . diag"    snd . diag = id
"fst . f &&& g" forall f g. fst . (f &&& g) = f
"snd . f &&& g" forall f g. snd . (f &&& g) = g
 #-}

instance PreCartesian (->) (,) where
    type Product (->) = (,)
    fst = Prelude.fst
    snd = Prelude.snd
    diag a = (a,a)
    (f &&& g) a = (f a, g a)

-- alias
class (Monoidal (~>) (Product (~>)), PreCartesian (~>)) => Cartesian (~>)
instance (Monoidal (~>) (Product (~>)), PreCartesian (~>)) => Cartesian (~>)

-- | free construction of 'Bifunctor' for the product 'Bifunctor' @Product k@ if @(&&&)@ is known
bimapProduct :: (PreCartesian (~>), (*) ~ Product (~>)) => (a ~> c) -> (b ~> d) -> (a * b) ~> (c * d)
bimapProduct f g = (f . fst) &&& (g . snd)
    
-- | free construction of 'Braided' for the product 'Bifunctor' @Product k@
braidProduct :: (PreCartesian (~>), (*) ~ Product (~>))  => a * b ~> b * a
braidProduct = snd &&& fst

-- | free construction of 'Associative' for the product 'Bifunctor' @Product k@
associateProduct :: (PreCartesian (~>), (*) ~ Product (~>))  => (a * b) * c ~> (a * (b * c))
associateProduct = (fst . fst) &&& first snd

-- | free construction of 'Coassociative' for the product 'Bifunctor' @Product k@
disassociateProduct:: (PreCartesian (~>),  (*) ~ Product (~>)) => a * (b * c) ~> (a * b) * c
disassociateProduct= braid . second braid . associateProduct . first braid . braid 

-- * Co-PreCartesian categories

-- a category that has finite coproducts, wea(~>)ened the same way as PreCartesian above was wea(~>)ened
class (Associative (~>) (Sum (~>)), Coassociative (~>) (Sum (~>)), Braided (~>) (Sum (~>))) => PreCoCartesian (~>) where
    type Sum (~>) :: * -> * -> *
    inl :: a ~> Sum (~>) a b
    inr :: b ~> Sum (~>) a b
    codiag :: Sum (~>) a a ~> a
    (|||) :: (a ~> c) -> (b ~> c) -> Sum (~>) a b ~> c

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

-- | free construction of 'Bifunctor' for the coproduct 'Bifunctor' @Sum (~>)@ if @(|||)@ is known
bimapSum :: (PreCoCartesian (~>), (+) ~ Sum (~>)) => (a ~> c) -> (b ~> d) -> (a + b) ~> (c + d)
bimapSum f g = (inl . f) ||| (inr . g)

-- | free construction of 'Braided' for the coproduct 'Bifunctor' @Sum (~>)@
braidSum :: (PreCoCartesian (~>), (+) ~ Sum (~>)) => (a + b) ~> (b + a)
braidSum = inr ||| inl

-- | free construction of 'Associative' for the coproduct 'Bifunctor' @Sum (~>)@
associateSum :: (PreCoCartesian (~>), (+) ~ Sum (~>)) => ((a + b) + c) ~> (a + (b + c))
associateSum = braid . first braid . disassociateSum . second braid . braid

-- | free construction of 'Coassociative' for the coproduct 'Bifunctor' @Sum (~>)@
disassociateSum :: (PreCoCartesian (~>), (+) ~ Sum (~>)) => (a + (b + c)) ~> ((a + b) + c)
disassociateSum = (inl . inl) ||| first inr

class (Comonoidal (~>) (Sum (~>)), PreCoCartesian (~>)) => CoCartesian (~>)
instance (Comonoidal (~>) (Sum (~>)), PreCoCartesian (~>)) => CoCartesian (~>)
