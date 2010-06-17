-- {-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}
-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Category.Associative
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-- NB: this contradicts another common meaning for an 'Associative' 'Category', which is one 
-- where the pentagonal condition does not hold, but for which there is an identity.
--
-------------------------------------------------------------------------------------------
module Control.Category.Associative 
	( Associative(..)
	, Coassociative(..)
	) where

import Control.Functor
import Control.Category.Hask

{- | A category with an associative bifunctor satisfying Mac Lane\'s pentagonal coherence identity law:

> bimap id associate . associate . bimap associate id = associate . associate
-}
class Bifunctor p k k k => Associative k p where
	associate :: k (p (p a b) c) (p a (p b c))

{- | A category with a coassociative bifunctor satisyfing the dual of Mac Lane's pentagonal coherence identity law:

> bimap coassociate id . coassociate . bimap id coassociate = coassociate . coassociate
-}
class Bifunctor s k k k => Coassociative k s where
	coassociate :: k (s a (s b c)) (s (s a b) c)

{-# RULES
"copentagonal coherence" bimap coassociate id . coassociate . bimap id coassociate = coassociate . coassociate
"pentagonal coherence" bimap id associate . associate . bimap associate id = associate . associate
 #-}

instance Associative Hask (,) where
        associate ((a,b),c) = (a,(b,c))

instance Coassociative Hask (,) where
        coassociate (a,(b,c)) = ((a,b),c)

instance Associative Hask Either where
        associate (Left (Left a)) = Left a
        associate (Left (Right b)) = Right (Left b)
        associate (Right c) = Right (Right c)

instance Coassociative Hask Either where
        coassociate (Left a) = Left (Left a)
        coassociate (Right (Left b)) = Left (Right b)
        coassociate (Right (Right c)) = Right c
