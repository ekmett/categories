{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
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
    ( PFunctor (first)
    , QFunctor (second)
    , Bifunctor (bimap)
    , dimap
    , difirst
    ) where

import Prelude hiding (id, (.))
import Control.Arrow (Kleisli(..))
import Control.Category
import Control.Category.Dual

class (Category r, Category t) => PFunctor p r t | p r -> t, p t -> r where
    first :: r a b -> t (p a c) (p b c)
--    default first :: Bifunctor p r s t => r a b -> t (p a c) (p b c)
--    first f = bimap f id

class (Category s, Category t) => QFunctor q s t | q s -> t, q t -> s where
    second :: s a b -> t (q c a) (q c b)
--    default second :: Bifunctor q r s t => s a b -> t (q c a) (q c b)
--    second = bimap id

-- | Minimal definition: @bimap@ 

-- or both @first@ and @second@
class (PFunctor p r t, QFunctor p s t) => Bifunctor p r s t | p r -> s t, p s -> r t, p t -> r s where
    bimap :: r a b -> s c d -> t (p a c) (p b d)
    -- bimap f g = second g . first f

instance PFunctor (,) (->) (->) where first f = bimap f id
instance QFunctor (,) (->) (->) where second = bimap id
instance Bifunctor (,) (->) (->) (->) where
    bimap f g (a,b)= (f a, g b)

instance PFunctor Either (->) (->) where first f = bimap f id
instance QFunctor Either (->) (->) where second = bimap id
instance Bifunctor Either (->) (->) (->) where
    bimap f _ (Left a) = Left (f a)
    bimap _ g (Right a) = Right (g a)

instance QFunctor (->) (->) (->) where
    second = (.)

instance Monad m => PFunctor (,) (Kleisli m) (Kleisli m) where first f = bimap f id
instance Monad m => QFunctor (,) (Kleisli m) (Kleisli m) where second f = bimap id f
instance Monad m => Bifunctor (,) (Kleisli m) (Kleisli m) (Kleisli m) where
  bimap (Kleisli f) (Kleisli g) = (Kleisli (\(a, c) -> (,) <$> f a <*> g c))

instance Monad m => PFunctor Either (Kleisli m) (Kleisli m) where first f = bimap f id
instance Monad m => QFunctor Either (Kleisli m) (Kleisli m) where second f = bimap id f
instance Monad m => Bifunctor Either (Kleisli m) (Kleisli m) (Kleisli m) where
  bimap (Kleisli f) (Kleisli g) = (Kleisli $ either (fmap Left . f) (fmap Right . g))

instance Monad m => QFunctor (Kleisli m) (Kleisli m) (Kleisli m) where
  second f = Kleisli $ return . (f .)

difirst :: PFunctor f (Dual s) t => s b a -> t (f a c) (f b c)
difirst = first . Dual

dimap :: Bifunctor f (Dual s) t u => s b a -> t c d -> u (f a c) (f b d)
dimap = bimap . Dual
