{-# OPTIONS -fglasgow-exts #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Object
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: non-portable (either class-associated types or MPTCs with fundeps)
--
-- This module declares the 'HasTerminalObject' and 'HasInitialObject' classes.
-- These are defined in terms of class-associated types rather than functional dependencies
-- because most of the time when you are manipulating a category you don't care about them;
-- this gets them out of the signature of most functions that use the category.
-- Both of these are special cases of the idea of a (co)limit.
-------------------------------------------------------------------------------------------

module Control.Category.Object 
	( HasTerminalObject(..)
	, HasInitialObject(..)
	) where

import Control.Category

-- | The @Category k@ has a terminal object @Terminal k@ such that for all objects @a@ in @k@, 
-- there exists a unique morphism from @a@ to @Terminal k@.
#ifdef USE_TYPE_FAMILIES
class Category k => HasTerminalObject k where
	type Terminal k :: *
	terminate :: k a (Terminal k)
#else 
class Category k => HasTerminalObject k t | k -> t where
	terminate :: k a t
#endif

-- | The @Category k@ has an initial (coterminal) object @Initial k@ such that for all objects 
-- @a@ in @k@, there exists a unique morphism from @Initial k @ to @a@.

#ifdef USE_TYPE_FAMILIES
class Category k => HasInitialObject k where
	type Initial k :: *
	initiate :: k (Initial k) a
#else
class Category k => HasInitialObject k i | k -> i where
	initiate :: k i a
#endif
