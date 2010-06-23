{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}
-------------------------------------------------------------------------------------------
-- |
-- Module   : Control.Categorical.Bifunctor
-- Copyright: 2008-2010 Edward Kmett
-- License  : BSD3
--
-- Maintainer : Edward Kmett <ekmett@gmail.com>
-- Stability  : experimental
-- Portability: non-portable (functional-dependencies)
--
-- A more categorical definition of 'Bifunctor'
-------------------------------------------------------------------------------------------
module Control.Categorical.Bifunctor
    ( PFunctor (first), firstDefault
    , QFunctor (second), secondDefault
    , Bifunctor (bimap)
    , dimap
    , difirst
    ) where

import Prelude hiding (id, (.))
import Control.Category
import Control.Category.Dual

class (Category r, Category t) => PFunctor p r t | p r -> t, p t -> r where
    first :: r a b -> t (p a c) (p b c)

instance PFunctor (,) (->) (->) where
    first f ~(a, b) = (f a, b)

instance PFunctor Either (->) (->) where
    first f (Left a)  = Left (f a)
    first _ (Right b) = Right b

{-# INLINE firstDefault #-}
firstDefault :: Bifunctor p r s t => r a b -> t (p a c) (p b c)
firstDefault f = bimap f id

difirst :: PFunctor f (Dual s) t => s b a -> t (f a c) (f b c)
difirst = first . Dual

class (Category s, Category t) => QFunctor q s t | q s -> t, q t -> s where
    second :: s a b -> t (q c a) (q c b)

{-# INLINE secondDefault #-}
secondDefault :: Bifunctor p r s t => s a b -> t (p c a) (p c b)
secondDefault = bimap id

instance QFunctor Either (->) (->) where
    second = secondDefault

instance Bifunctor Either (->) (->) (->) where
    bimap f _ (Left a) = Left (f a)
    bimap _ g (Right a) = Right (g a)

instance QFunctor (->) (->) (->) where
    second = (.)

instance QFunctor (,) (->) (->) where
    second = secondDefault

instance Bifunctor (,) (->) (->) (->) where
    bimap f g ~(a,b)= (f a, g b)

class (PFunctor p r t, QFunctor p s t) => Bifunctor p r s t | p r -> s t, p s -> r t, p t -> r s where
    bimap :: r a b -> s c d -> t (p a c) (p b d)

dimap :: Bifunctor f (Dual s) t u => s b a -> t c d -> u (f a c) (f b d)
dimap = bimap . Dual

