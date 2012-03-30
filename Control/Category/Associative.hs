{-# LANGUAGE MultiParamTypeClasses #-}
-------------------------------------------------------------------------------------------
-- |
-- Module    : Control.Category.Associative
-- Copyright : 2008 Edward Kmett
-- License   : BSD
--
-- Maintainer  : Edward Kmett <ekmett@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- NB: this contradicts another common meaning for an 'Associative' 'Category', which is one
-- where the pentagonal condition does not hold, but for which there is an identity.
--
-------------------------------------------------------------------------------------------
module Control.Category.Associative
    ( Associative(..)
    ) where

import Control.Categorical.Bifunctor

{- | A category with an associative bifunctor satisfying Mac Lane\'s pentagonal coherence identity law:

> bimap id associate . associate . bimap associate id = associate . associate
> bimap disassociate id . disassociate . bimap id disassociate = disassociate . disassociate
-}
class Bifunctor p k k k => Associative k p where
    associate :: k (p (p a b) c) (p a (p b c))
    disassociate :: k (p a (p b c)) (p (p a b) c)

{-- RULES
"copentagonal coherence" first disassociate . disassociate . second disassociate = disassociate . disassociate
"pentagonal coherence"   second associate . associate . first associate = associate . associate
 --}

instance Associative (->) (,) where
        associate ((a,b),c) = (a,(b,c))
        disassociate (a,(b,c)) = ((a,b),c)

instance Associative (->) Either where
        associate (Left (Left a)) = Left a
        associate (Left (Right b)) = Right (Left b)
        associate (Right c) = Right (Right c)
        disassociate (Left a) = Left (Left a)
        disassociate (Right (Left b)) = Left (Right b)
        disassociate (Right (Right c)) = Right c
