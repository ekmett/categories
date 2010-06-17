{-# OPTIONS -fglasgow-exts #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Cartesian.Closed
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ehommett@gmail.com>
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

-- Ideally you also want an instance for @'Bifunctor' ('Exp' hom) ('Dual' hom) hom hom@.
-- or at least @'Functor' ('Exp' hom a) hom hom@, which cannot be expressed in the constraints here.

class (Monoidal hom prod i, Cartesian hom prod i) => CCC hom prod exp i | hom -> prod exp i where
	apply :: hom (prod (exp a b) a) b
	curry :: hom (prod a b) c -> hom a (exp b c)
	uncurry :: hom a (exp b c) -> hom (prod a b) c

{-# RULES
"curry apply" 		curry apply = id
-- "curry . uncurry" 	curry . uncurry = id :: CCC hom => hom a (exp b c) -> hom a (exp b c)
-- "uncurry . curry" 	uncurry . curry = id :: CCC hom => hom (prod a b) c -> hom (prod a b) c
 #-}

-- * Free 'Adjunction' (prod a) (exp a) hom hom 

unitCCC :: CCC hom prod exp i => hom a (exp b (prod b a))
unitCCC = curry braid

counitCCC :: CCC hom prod exp i => hom (prod b (exp b a)) a
counitCCC = apply . braid

-- * A Co-(Closed Cartesian Category) 

-- | A Co-CCC has full-fledged comonoidal finite coproducts and coexponentials

-- You probably also want an instance for @'Bifunctor' ('coexp' hom) ('Dual' hom) hom hom@.

class (Comonoidal hom sum i, CoCartesian hom sum i) => CoCCC hom sum coexp i | hom -> sum coexp i where
	coapply :: hom b (sum (coexp hom a b) a)
	cocurry :: hom c (sum a b) -> hom (coexp hom b c) a
	uncocurry :: hom (coexp hom b c) a -> hom c (sum a b)

{-# RULES
"cocurry coapply" 	   cocurry coapply = id
-- "cocurry . uncocurry"   cocurry . uncocurry = id
-- "uncocurry . cocurry"   uncocurry . cocurry = id
 #-}

-- * Free 'Adjunction' (coexp hom a) (sum a) hom hom 

unitCoCCC :: CoCCC hom sum coexp i => hom a (sum b (coexp hom b a))
unitCoCCC = braid . coapply

counitCoCCC :: CoCCC hom sum coexp i => hom (coexp hom b (sum b a)) a
counitCoCCC = cocurry braid
