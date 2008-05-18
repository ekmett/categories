{-# OPTIONS -fglasgow-exts #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Cartesian.Closed
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (class-associated types)
--
-- NB: Some rewrite rules are disabled pending resolution of:
-- <http://hackage.haskell.org/trac/ghc/ticket/2291>
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

-- Ideally you also want an instance for @'Bifunctor' ('Exp' k) ('Dual' k) k k@.
-- or at least @'Functor' ('Exp' k a) k k@, which cannot be expressed in the constraints here.

class (Monoidal (Prod k) k, Cartesian k) => CCC k where
#ifndef __HADDOCK__
	type Exp k :: * -> * -> *
#endif
	apply :: k (Prod k (Exp k a b) a) b
	curry :: k (Prod k a b) c -> k a (Exp k b c)
	uncurry :: k a (Exp k b c) -> k (Prod k a b) c

{-# RULES
"curry apply" 		curry apply = id
-- "curry . uncurry" 	curry . uncurry = id :: CCC k => k a (Exp k b c) -> k a (Exp k b c)
-- "uncurry . curry" 	uncurry . curry = id :: CCC k => k (Prod k a b) c -> k (Prod k a b) c
 #-}

-- * Free 'Adjunction' (Prod k a) (Exp k a) k k 

unitCCC :: CCC k => k a (Exp k b (Prod k b a))
unitCCC = curry braid

counitCCC :: CCC k => k (Prod k b (Exp k b a)) a
counitCCC = apply . braid

-- * A Co-(Closed Cartesian Category) 

-- | A Co-CCC has full-fledged comonoidal finite coproducts and coexponentials

-- You probably also want an instance for @'Bifunctor' ('Coexp' k) ('Dual' k) k k@.

class (Comonoidal (Sum k) k, CoCartesian k) => CoCCC k where
#ifndef __HADDOCK__
	type Coexp k :: * -> * -> *
#endif
	coapply :: k b (Sum k (Coexp k a b) a)
	cocurry :: k c (Sum k a b) -> k (Coexp k b c) a
	uncocurry :: k (Coexp k b c) a -> k c (Sum k a b)

{-# RULES
"cocurry coapply" 	cocurry coapply = id
-- "cocurry . uncocurry"	cocurry . uncocurry = id
-- "uncocurry . cocurry"   uncocurry . cocurry = id
 #-}

-- * Free 'Adjunction' (Coexp k a) (Sum k a) k k 

unitCoCCC :: CoCCC k => k a (Sum k b (Coexp k b a))
unitCoCCC = braid . coapply

counitCoCCC :: CoCCC k => k (Coexp k b (Sum k b a)) a
counitCoCCC = cocurry braid
