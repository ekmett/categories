{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	 : Control.Category.Cartesian.Closed
-- Copyright : 2008 Edward Kmett
-- License	 : BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-------------------------------------------------------------------------------------------
module Control.Category.Cartesian.Closed
	( 
	-- * Cartesian Closed Category
	  CCC(..)
	, unitCCC, counitCCC
	-- * Co-(Cartesian Closed Category)
	, CoCCC(..)
	, unitCoCCC, counitCoCCC
	) where

import Prelude hiding ((.), id, fst, snd, curry, uncurry)

import Control.Category
import Control.Category.Cartesian
import Control.Category.Monoidal

-- * Closed Cartesian Category 

-- | A 'CCC' has full-fledged monoidal finite products and exponentials

-- Ideally you also want an instance for @'Bifunctor' ('Exp' hom) ('Dual' hom) hom hom@.
-- or at least @'Functor' ('Exp' hom a) hom hom@, which cannot be expressed in the constraints here.

class ( Cartesian (<=)
      , (*) ~ Product (<=) 
      , Symmetric (<=) (*)
      , Monoidal (<=) (*) 
      , (\) ~ Exp (<=) 
    ) => CCC (<=) where
    type Exp (<=) :: * -> * -> *
	apply :: ((a \ b) * a) <= b
	curry :: ((a * b) <= c) -> a <= (b \ c)
	uncurry :: (a <= (b \ c)) -> ((a * b) <= c)

{-# RULES
"curry apply" 		curry apply = id
-- "curry . uncurry" 	curry . uncurry = id
-- "uncurry . curry" 	uncurry . curry = id
 #-}

-- * Free @'Adjunction' (Product (<=) a) (Exp (<=) a) (<=) (<=)@

unitCCC :: (CCC (<=), (*) ~ Product (<=), (\) ~ Exp (<=)) => a <= b \ (b * a)
unitCCC = curry braid

counitCCC :: (CCC (<=), (*) ~ Product (<=), (\) ~ Exp (<=)) => (b * (b \ a)) <= a
counitCCC = apply . braid

-- * A Co-(Closed Cartesian Category) 

-- | A Co-CCC has full-fledged comonoidal finite coproducts and coexponentials

-- You probably also want an instance for @'Bifunctor' ('coexp' hom) ('Dual' hom) hom hom@.

class 
    ( CoCartesian (<=),
    , (+) ~ Sum (<=) 
    , Symmetric (<=) (+)
    , Comonoidal (<=) (+)
    , subtract ~ Coexp (<=)
    ) => CoCCC (<=) where
    type Coexp (<=) :: * -> * -> *
	coapply :: b <= subtract a b + a
	cocurry :: (c <= (a + b)) -> (subtract b c <= a)
	uncocurry :: (subtract b c <= a) -> (c <= (a + b))

{-# RULES
"cocurry coapply" 	   cocurry coapply = id
-- "cocurry . uncocurry"   cocurry . uncocurry = id
-- "uncocurry . cocurry"   uncocurry . cocurry = id
 #-}

-- * Free @'Adjunction' ('Coexp' (<=) a) ('Sum' (<=) a) (<=) (<=)@
unitCoCCC :: CoCCC (<=), subtract ~ Coexp (<=), (+) ~ Sum (<=)) => a <= b + subtract b a
unitCoCCC = swap . coapply

counitCoCCC :: CoCCC (<=), subtract ~ Coexp (<=), (+) ~ Sum (<=)) => subtract b (b + a) <= a
counitCoCCC = cocurry swap
