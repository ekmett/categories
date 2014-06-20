{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
module Group where

import Data.Tagged
import Data.Void
import qualified Data.Functor.Contravariant as Contra
import Control.Category (Category(..))
import qualified Control.Arrow as Arrow
import qualified Prelude
import Prelude (Either(..), ($), either)

type family (~>) :: i -> i -> *
type instance (~>) = (->)
type instance (~>) = Nat
infixr 0 ~>
infixl 6 +
infixl 7 *

newtype Nat f g = Nat { runNat :: forall a. f a -> g a }

instance Category Nat where
  id = Nat id
  Nat f . Nat g = Nat (f . g)

class Functor (f :: x -> y) where
  fmap :: (a ~> b) -> f a ~> f b

instance Prelude.Functor f => Functor f where
  fmap = Prelude.fmap

instance Functor (Nat f) where
  fmap = (.)

newtype At (x :: i) (f :: i -> *) = At { getAt :: f x }
_At = dimap getAt At

instance Functor (At x) where
  fmap (Nat f) = _At f

newtype Const (b :: *) (a :: i) = Const { getConst :: b }
_Const = dimap getConst Const
_Tagged = dimap unTagged Tagged

instance Functor Const where
  fmap f = Nat (_Const f)

newtype Lift (p :: * -> * -> *) (f :: i -> *) (g :: i -> *) (a :: i) = Lift { lower :: p (f a) (g a) }
_Lift = dimap lower Lift

class Contravariant (f :: x -> y) where
  contramap :: (b ~> a) -> f a ~> f b

instance Contra.Contravariant f => Contravariant f where
  contramap = Contra.contramap

class PFunctor (p :: x -> y -> z) where
  first :: (a ~> b) -> p a c ~> p b c

instance PFunctor (,) where first = Arrow.first
instance PFunctor Either where first = Arrow.left
instance PFunctor p => PFunctor (Lift p) where
  first (Nat f) = Nat (_Lift $ first f)
instance PFunctor Tagged where first _ = _Tagged id

class QFunctor (p :: x -> y -> z) where
  second :: (a ~> b) -> p c a ~> p c b

instance QFunctor (->) where second = (.)
instance QFunctor Nat where second = (.)
instance QFunctor (,) where second = Arrow.second
instance QFunctor Either where second = Arrow.right
instance QFunctor (Const :: * -> i -> *) where second _ = _Const id
instance QFunctor p => QFunctor (Lift p) where
  second (Nat f) = Nat (_Lift $ second f)
instance QFunctor At where
  second (Nat f) = _At f
instance QFunctor Tagged where second = Prelude.fmap

class PContravariant (p :: x -> y -> z) where
  lmap :: (a ~> b) -> p b c ~> p a c

instance PContravariant (->) where lmap f g = g . f
instance PContravariant Nat where lmap f g = g . f
instance PContravariant p => PContravariant (Lift p) where lmap (Nat f) = Nat $ _Lift (lmap f)
instance PContravariant Tagged where lmap _ = _Tagged id

class QContravariant (p :: x -> y -> z) where
  qmap :: (a ~> b) -> p c b ~> p c a

instance QContravariant (Const :: * -> i -> *) where qmap _ = _Const id
instance QContravariant p => QContravariant (Lift p) where qmap (Nat f) = Nat $ _Lift (qmap f)
instance Contravariant (Const k :: i -> *) where contramap _ = _Const id

class (PFunctor p, QFunctor p, Category ((~>):: z -> z -> *)) => Bifunctor (p :: x -> y -> z)
instance (PFunctor p, QFunctor p, Category ((~>):: z -> z -> *)) => Bifunctor (p :: x -> y -> z)

type Review t b = forall p. Bifunctor p => p b b -> p t t

unto :: (b ~> t) -> Review t b
unto f = bimap f f

class (PContravariant p, QFunctor p, Category ((~>) :: z -> z -> *)) => Profunctor (p :: x -> y -> z)
instance (PContravariant p, QFunctor p, Category ((~>) :: z -> z -> *)) => Profunctor (p :: x -> y -> z)

type Iso s t a b = forall p. Profunctor p => p a b -> p s t

class (PContravariant p, QContravariant p, Category ((~>) :: z -> z -> *)) => Bicontravariant (p :: x -> y -> z)
instance (PContravariant p, QContravariant p, Category ((~>) :: z -> z -> *)) => Bicontravariant (p :: x -> y -> z)

bicontramap :: Bicontravariant p => (a ~> b) -> (c ~> d) -> p b d ~> p a c
bicontramap f g = lmap f . qmap g

type Getter s a = forall p. Bicontravariant p => p a a -> p s s

to :: (s ~> a) -> Getter s a
to f = bicontramap f f

newtype Viewing r a b = Viewing { runViewing :: a ~> r }
_Viewing = dimap runViewing Viewing

instance Category ((~>) :: i -> i -> *) => PContravariant (Viewing (r :: i)) where lmap f = _Viewing (. f)
instance QContravariant (Viewing r) where qmap _ = _Viewing id
instance QFunctor (Viewing r) where second _ = _Viewing id

view :: forall (s::i) (a::i). Category ((~>) :: i -> i -> *) => (Viewing a a a -> Viewing a s s) -> s ~> a
view l = runViewing $ l (Viewing id)

review :: forall (t::i) (b::i). Category ((~>) :: i -> i -> *) => (Reviewing b b b -> Reviewing b t t) -> b ~> t
review l = runReviewing $ l (Reviewing id)

newtype Reviewing r a b = Reviewing { runReviewing :: r ~> b }
_Reviewing = dimap runReviewing Reviewing

instance PFunctor (Reviewing r) where first _ = _Reviewing id
instance PContravariant (Reviewing r) where lmap _ = _Reviewing id
instance Category ((~>) :: i -> i -> *) => QFunctor (Reviewing (r :: i)) where second f = _Reviewing (f .)

class (PContravariant p, PFunctor p) => PPhantom (p :: x -> y -> z)
instance (PContravariant p, PFunctor p) => PPhantom (p :: x -> y -> z)

class (QContravariant p, QFunctor p) => QPhantom (p :: x -> y -> z)
instance (QContravariant p, QFunctor p) => QPhantom (p :: x -> y -> z)

rmap :: QFunctor p => (a ~> b) -> p c a ~> p c b
rmap = second

bimap :: (Category ((~>) :: z -> z -> *), Bifunctor (p :: x -> y -> z)) => (a ~> b) -> (c ~> d) -> p a c ~> p b d
bimap f g = first f . second g

dimap :: (Category ((~>) :: z -> z -> *), Profunctor (p :: x -> y -> z)) => (a ~> b) -> (c ~> d) -> p b c ~> p a d
dimap f g = lmap f . rmap g

-- tensor for a skew monoidal category
class Bifunctor p => Tensor (p :: x -> x -> x) where
  type Id p :: x
  associate :: p (p a b) c ~> p a (p b c)
  lambda    :: p (Id p) a ~>  a
  rho       :: a ~> p a (Id p)

instance Tensor (,) where
  type Id (,) = ()
  associate ((a,b),c) = (a,(b,c))
  lambda    ((),a)    = a
  rho       a         = (a ,())

instance Tensor Either where
  type Id Either = Void
  associate (Left (Left a)) = Left a
  associate (Left (Right b)) = Right (Left b)
  associate (Right c) = Right (Right c)
  lambda (Right a) = a
  rho = Left

instance Tensor p => Tensor (Lift p) where
  type Id (Lift p) = Const (Id p)
  associate = Nat $ _Lift $ second Lift . associate . first lower
  lambda    = Nat $ lmap (first getConst . lower) lambda
  rho       = Nat $ rmap (Lift . second Const) rho

-- symmetric monoidal category
class Bifunctor p => Symmetric p where
  swap :: p a b ~> p b a

instance Symmetric (,) where
  swap (a,b) = (b, a)

instance Symmetric Either where
  swap = either Right Left

instance Symmetric p => Symmetric (Lift p) where
  swap = Nat $ _Lift swap

-- profunctor composition
data Prof :: (j -> k -> *) -> (i -> j -> *) -> i -> k -> * where
  Prof :: forall (p :: j -> k -> *) (q :: i -> j -> *) (a :: i) (b :: j) (c :: k). p b c -> q a b -> Prof p q a c

associateProf :: forall (p :: k -> l -> *) (q :: j -> k -> *) (r :: i -> j -> *) (a :: i) (c :: l). Prof (Prof p q) r a c -> Prof p (Prof q r) a c
associateProf (Prof (Prof a b) c) = Prof a (Prof b c)

lambdaProf :: forall (p :: i -> j -> *) (a :: i) (b :: j). QFunctor p => Prof (~>) p a b -> p a b
lambdaProf (Prof h p) = rmap h p

rhoProf :: forall (p :: i -> k -> *) (a :: i) (b :: k). Category ((~>) :: i -> i -> *) => p a b -> Prof p (~>) a b
rhoProf p = Prof p id

class Terminal t where
  terminal :: a ~> t

instance Terminal () where
  terminal _ = ()

instance Terminal (Const ()) where
  terminal = Nat $ \_ -> Const ()

class Initial t where
  initial :: t ~> a

instance Initial Void where
  initial = absurd

instance Initial (Const Void) where
  initial = Nat $ absurd . getConst

class (h ~ (~>), Symmetric ((*)::i->i->i), Tensor ((*)::i->i->i), Terminal (Id ((*)::i->i->i))) => Cartesian (h :: i -> i -> *) | i -> h where
  type (*) :: i -> i -> i
  fst   :: forall (a :: i) (b :: i). a * b ~> a
  snd   :: forall (a :: i) (b :: i). a * b ~> b
  (&&&) :: forall (a :: i) (b :: i) (c :: i). (a ~> b) -> (a ~> c) -> a ~> b * c

instance Cartesian (->) where
  type (*) = (,)
  fst   = Prelude.fst
  snd   = Prelude.snd
  (&&&) = (Arrow.&&&)

instance Cartesian Nat where
  type (*) = Lift (,)
  fst = Nat $ fst . lower
  snd = Nat $ snd . lower
  Nat f &&& Nat g = Nat $ Lift . (f &&& g)

class (Cartesian ((~>) :: i -> i -> *), Profunctor p) => Strong (p :: i -> i -> *) where
  {-# MINIMAL _1 | _2 #-}
  _1 :: p a b -> p (a * c) (b * c)
  _1 = dimap swap swap . _2
  _2 :: p a b -> p (c * a) (c * b)
  _2 = dimap swap swap . _1

instance Strong (->) where
  _1 = first
  _2 = second

instance Strong Nat where
  _1 = first
  _2 = second

instance Cartesian ((~>) :: i -> i -> *) => Strong (Viewing (r :: i)) where
  _1 = _Viewing (. fst)

type Lens s t a b = forall p. Strong p => p a b -> p s t

class (h ~ (~>), Symmetric ((+)::i->i->i), Tensor ((+)::i->i->i),Initial (Id ((+)::i->i->i))) => Cocartesian (h :: i -> i -> *) | i -> h where
  type (+) :: i -> i -> i
  inl    :: forall (a :: i) (b :: i). a ~> a + b
  inr    :: forall (a :: i) (b :: i). b ~> a + b
  (|||)  :: forall (a :: i) (b :: i) (c :: i). (a ~> c) -> (b ~> c) -> a + b ~> c

instance Cocartesian (->) where
  type (+) = Either
  inl = Left
  inr = Right
  (|||) = either

instance Cocartesian Nat where
  type (+) = Lift Either
  inl = Nat (Lift . Left)
  inr = Nat (Lift . Right)
  Nat f ||| Nat g = Nat $ either f g . lower

class (Cocartesian ((~>) :: i -> i -> *), Profunctor p) => Choice (p :: i -> i -> *) where
  {-# MINIMAL _Left | _Right #-}
  _Left  :: p a b -> p (a + c) (b + c)
  _Left = dimap swap swap . _Right
  _Right :: p a b -> p (c + a) (c + b)
  _Right = dimap swap swap . _Left

instance Choice (->) where
  _Left = Arrow.left
  _Right = Arrow.right

instance Choice Nat where
  _Left (Nat f) = Nat $ _Lift (_Left f)
  _Right (Nat g) = Nat $ _Lift (_Right g)

instance Choice Tagged where
  _Left  = bimap inl inl
  _Right = bimap inr inr

instance Cocartesian ((~>) :: i -> i -> *) => Choice (Reviewing (r :: i)) where
  _Left = bimap inl inl
  _Right = bimap inr inr

type Prism s t a b = forall p. Choice p => p a b -> p s t

class (Profunctor (Exp :: x -> x -> x), Cartesian k) => CCC (k :: x -> x -> *) | x -> k where
  type Exp :: x -> x -> x
  curried :: forall (a :: x) (b :: x) (c :: x) (a' :: x) (b' :: x) (c' :: x). Iso (a * b ~> c) (a' * b' ~> c') (a ~> Exp b c) (a' ~> Exp b' c')

instance CCC (->) where
  type Exp = (->)
  curried = dimap Prelude.curry Prelude.uncurry

instance CCC Nat where
  type Exp = Lift (->)
  curried = dimap hither yon where
    hither (Nat f) = Nat $ \a -> Lift $ \b -> f (Lift (a, b))
    yon (Nat f) = Nat $ \(Lift (a,b)) -> lower (f a) b
