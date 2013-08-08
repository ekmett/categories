{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}
-------------------------------------------------------------------------------------------
-- |
-- Copyright: 2008-2013 Edward Kmett
-- License  : BSD3
--
-- Maintainer : Edward Kmett <ekmett@gmail.com>
-- Stability  : experimental
-- Portability: non-portable (functional-dependencies)
--
-- A more categorical definition of 'Bifunctor'
-------------------------------------------------------------------------------------------
module Control.Categorical.Bifunctor
    ( PFunctor (first)
    , QFunctor (second)
    , Bifunctor (bimap)
    , dimap
    , difirst
    ) where

import Prelude hiding (id, (.))
import Control.Categorical.Category

class (Category r, Category t) => PFunctor (p :: x -> y -> z) (r :: x -> x -> *) (t :: z -> z -> *) | p r -> t, p t -> r where
    first :: r a b -> t (p a c) (p b c)
    default first :: Bifunctor p r s t => r a b -> t (p a c) (p b c)
    first f = bimap f id

-- | 'contramap' over the first argument of a 'Bifunctor' from D^op x C -> E
difirst :: PFunctor p (Dual r) t => r b a -> t (p a c) (p b c)
difirst = first . Dual

class (Category s, Category t) => QFunctor (q :: x -> y -> z) (s :: y -> y -> *) (t :: z -> z -> *) | q s -> t, q t -> s where
    second :: s a b -> t (q c a) (q c b)
    default second :: Bifunctor q r s t => s a b -> t (q c a) (q c b)
    second = bimap id

-- | Minimal definition: @bimap@

-- or both @first@ and @second@
class (PFunctor p r t, QFunctor p s t) => Bifunctor p r s t | p r -> s t, p s -> r t, p t -> r s where
    bimap :: r a b -> s c d -> t (p a c) (p b d)
    bimap f g = first f . second g

-- | 'contramap' over the first argument of a 'Bifunctor' and 'map' over the second.
dimap :: Bifunctor p (Dual r) s t => r b a -> s c d -> t (p a c) (p b d)
dimap = bimap . Dual

instance PFunctor (,) (->) (->)
instance QFunctor (,) (->) (->)
instance Bifunctor (,) (->) (->) (->) where
    bimap f g (a,b)= (f a, g b)

instance PFunctor Either (->) (->)
instance QFunctor Either (->) (->)
instance Bifunctor Either (->) (->) (->) where
    bimap f _ (Left a) = Left (f a)
    bimap _ g (Right a) = Right (g a)

instance QFunctor (->) (->) (->) where
    second = (.)


