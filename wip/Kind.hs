{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
module Kind where

import qualified Control.Applicative as Applicative
import qualified Control.Arrow as Arrow
import Control.Category (Category(..))
import qualified Control.Comonad as Comonad
import qualified Control.Monad as Monad
import qualified Data.Constraint as Constraint
import Data.Constraint ((:-)(Sub), (\\), Dict(Dict))
import qualified Data.Functor.Contravariant as Contravariant
import Data.Functor.Identity
import qualified Data.Monoid as Monoid
import Data.Proxy
import Data.Tagged
import qualified Data.Traversable as Traversable
import Data.Void
import qualified Prelude
import Prelude (Either(..), ($), either, Bool, undefined)
import GHC.Exts (Constraint, Any)
import Unsafe.Coerce (unsafeCoerce)

-- * A kind-indexed family of categories

infixr 0 ~>

type family (~>) :: i -> i -> *
type instance (~>) = (->)  -- @* -> * -> *@
type instance (~>) = Nat   -- @(i -> j) -> (i -> j) -> *@
type instance (~>) = (:-)  -- @Constraint -> Constraint -> *@
type instance (~>) = Unit  -- @() -> () -> *@
type instance (~>) = Empty -- @Void -> Void -> *@
type instance (~>) = Prod  -- @(i,j) -> (i, j) -> *@

-- * convenience types that make it so we can avoid explicitly talking about the kinds as much as possible
type Dom  (f :: x -> y)      = ((~>) :: x -> x -> *)
type Cod  (f :: x -> y)      = ((~>) :: y -> y -> *)
type Cod2 (f :: x -> y -> z) = ((~>) :: z -> z -> *)
type Arr  (a :: x)           = ((~>) :: x -> x -> *)


newtype Nat f g = Nat { runNat :: forall a. f a ~> g a }

instance Category ((~>) :: j -> j -> *) => Category (Nat :: (i -> j) -> (i -> j) -> *) where
  id = Nat id
  Nat f . Nat g = Nat (f . g)

-- * Functors between these kind-indexed categories

class Functor (f :: x -> y) where
  fmap :: (a ~> b) -> f a ~> f b

class Contravariant (f :: x -> y) where
  contramap :: (b ~> a) -> f a ~> f b

first :: Functor p => (a ~> b) -> p a c ~> p b c
first = runNat . fmap

lmap :: Contravariant p => (a ~> b) -> p b c ~> p a c
lmap = runNat . contramap

class Functor1 (p :: x -> y -> z) where
  fmap1 :: (a ~> b) -> p c a ~> p c b

class Contravariant1 (p :: x -> y -> z) where
  contramap1 :: (a ~> b) -> p c b ~> p c a

-- | the isomorphism that witnesses f -| u
type (f :: y -> x) -: (u :: x -> y) = forall a b a' b'. Iso (f a ~> b) (f a' ~> b') (a ~> u b) (a' ~> u b')

-- | @f -| u@ indicates f is left adjoint to u
class (Functor f, Functor u) => (f::y->x) -| (u::x->y) | f -> u, u -> f where
  adj :: f -: u

unitAdj :: (f -| u, Category (Dom u)) => a ~> u (f a)
unitAdj = get adj id

counitAdj :: (f -| u, Category (Dom f)) => f (u b) ~> b
counitAdj = unget adj id

-- given f -| u, u is a strong monoidal functor
--
-- @
-- ap0 = get adj terminal
-- ap2 = get zipR
-- @
zipR :: (f -| u, Cartesian (Dom u), Cartesian (Cod u))
     => Iso (u a * u b) (u a' * u b') (u (a * b)) (u (a' * b'))
zipR = dimap (get adj (unget adj fst &&& unget adj snd))
             (fmap fst &&& fmap snd)

absurdL :: (f -| u, Initial z) => Iso' z (f z)
absurdL = dimap initial (unget adj initial)

cozipL :: (f -| u, Cocartesian (Dom u), Cocartesian (Cod u))
       => Iso (f (a + b)) (f (a' + b')) (f a + f b) (f a' + f b')
cozipL = dimap
  (unget adj (get adj inl ||| get adj inr))
  (fmap inl ||| fmap inr)


-- tabulated :: (f -| u) => Iso (a ^ f One) (b ^ f One) (u a) (u b)
-- splitL :: (f -| u) => Iso (f a) (f a') (a * f One) (a' * f One)

-- * common aliases
class (Functor p, Functor1 p, Category (Cod2 p)) => Bifunctor p
instance (Functor p, Functor1 p, Category (Cod2 p)) => Bifunctor p

class (Contravariant p, Contravariant1 p, Category (Cod2 p)) => Bicontravariant p
instance (Contravariant p, Contravariant1 p, Category (Cod2 p)) => Bicontravariant p

-- enriched profuncors C^op * D -> E
class (Contravariant p, Functor1 p, Cartesian (Cod2 p)) => Profunctor p
instance (Contravariant p, Functor1 p, Cartesian (Cod2 p)) => Profunctor p

-- Lift Prelude instances of Functor without overlap, using the kind index to say
-- these are all the instances of kind * -> *
instance Prelude.Functor f => Functor f where
  fmap = Prelude.fmap

instance Contravariant.Contravariant f => Contravariant f where
  contramap = Contravariant.contramap

instance Category ((~>) :: j -> j -> *) => Functor (Nat f :: (i -> j) -> *) where
  fmap = (.)

instance Functor ((:-) f) where
  fmap = fmap1

-- We can defne a functor from the category of natural transformations to Hask
newtype At (x :: i) (f :: i -> *) = At { getAt :: f x }
_At = dimap getAt At

instance Functor (At x) where
  fmap (Nat f) = _At f

instance Monoidal (At x) where
  ap0 = At . Const
  ap2 (At fx, At fy) = At (Lift (fx, fy))

instance Monoid m => Monoid (At x m) where
  one = oneM
  mult = multM

-- .. and back
class Const ~ k => Constant (k :: j -> i -> j) | j i -> k where
  type Const :: j -> i -> j
  _Const :: Iso (k b a) (k b' a') b b'

newtype Const1 b a = Const { getConst :: b }

instance Constant Const1 where
  type Const = Const1
  _Const = dimap getConst Const

instance Functor Const1 where
  fmap f = Nat (_Const f)

instance Functor1 Const1 where
  fmap1 _ = _Const id

instance Functor (Const1 b) where
  fmap = fmap1

newtype Const2 (f :: j -> *) (a :: i) (c :: j) = Const2 { getConst2 :: f c }

instance Constant Const2 where
  type Const = Const2
  _Const = dimap (Nat getConst2) (Nat Const2)

instance Functor Const2 where
  fmap f = Nat (_Const f)

instance Functor1 Const2 where
  fmap1 _ = Nat $ Const2 . getConst2

instance Functor (Const2 f) where
  fmap = fmap1

instance Functor f => Functor1 (Const2 f) where
  fmap1 f = Const2 . fmap f . getConst2

instance Functor f => Functor (Const2 f a) where
  fmap = fmap1

class b => ConstC b a
instance b => ConstC b a

instance Constant ConstC where
  type Const = ConstC
  _Const = dimap (Sub Dict) (Sub Dict)

instance Functor ConstC where
  fmap f = Nat (_Const f)

instance Functor (ConstC b) where
  fmap _ = Sub Dict

-- * -^J -| Limit

type family Limit :: (i -> j) -> j

newtype Limit1 (f :: i -> *) = Limit { getLimit :: forall x. f x }
type instance Limit = Limit1

instance Functor Limit1 where
  fmap (Nat f) (Limit g) = Limit (f g)

instance Monoidal Limit1 where
  ap0 () = Limit $ Const ()
  ap2 (Limit f, Limit g) = Limit (Lift (f, g))

instance Monoid m => Monoid (Limit1 m) where
  one = oneM
  mult = multM

instance Const1 -| Limit1 where
  adj = dimap (\f a -> Limit (runNat f (Const a))) $ \h -> Nat $ getLimit . h . getConst

newtype Limit2 (f :: i -> j -> *) (y :: j) = Limit2 { getLimit2 :: forall x. f x y }
type instance Limit = Limit2

instance Functor Limit2 where
  fmap f = Nat $ \(Limit2 g) -> Limit2 (runNat (runNat f) g)

-- instance Monoidal Limit2 -- instantiate when Nat on 2 arguments is made Cartesian

instance Const2 -| Limit2 where
  adj = dimap (\(Nat f) -> Nat $ \ a -> Limit2 (runNat f (Const2 a))) $ \(Nat h) -> Nat $ Nat $ getLimit2 . h . getConst2

-- has to abuse Any because any inhabits every kind, but it is not a good choice of Skolem!
class LimitC (p :: i -> Constraint) where
  limitDict :: Dict (p a)

instance p Any => LimitC (p :: i -> Constraint) where
  limitDict = case unsafeCoerce (id :: p Any :- p Any) :: p Any :- p a of
    Sub d -> d

type instance Limit = LimitC

instance Functor LimitC where
  fmap f = dimap (Sub limitDict) (Sub Dict) (runAny f) where
    runAny :: (p ~> q) -> p Any ~> q Any
    runAny = runNat

instance Monoidal LimitC where
  ap0 = Sub Dict
  ap2 = get zipR

instance Monoid m => Monoid (LimitC m) where
  one = oneM
  mult = multM

instance ConstC -| LimitC where
  adj = dimap (hither . runNat) (\b -> Nat $ dimap (Sub Dict) (Sub limitDict) b) where
    hither :: (ConstC a Any :- f Any) -> a :- LimitC f
    hither = dimap (Sub Dict) (Sub Dict)

-- * Colimit -| -^J

type family Colimit :: (i -> j) -> j
type instance Colimit = Colimit1
type instance Colimit = Colimit2

data Colimit1 (f :: i -> *) where
  Colimit :: f x -> Colimit1 f

instance Functor Colimit1 where
  fmap (Nat f) (Colimit g)= Colimit (f g)

instance Opmonoidal Colimit1 where
  op0 (Colimit (Const a)) = a
  op2 (Colimit (Lift ab)) = bimap Colimit Colimit ab

instance Comonoid m => Comonoid (Colimit1 m) where
  zero = zeroOp
  comult = comultOp

instance Colimit1 -| Const1 where
  adj = dimap (\f -> Nat $ Const . f . Colimit) $ \(Nat g2cb) (Colimit g) -> getConst (g2cb g)

data Colimit2 (f :: i -> j -> *) (x :: j) where
  Colimit2 :: f y x -> Colimit2 f x

instance Functor Colimit2 where
  fmap f = Nat $ \(Colimit2 g) -> Colimit2 (runNat (runNat f) g)

-- instance Opmonoidal Colimit2
-- instance Comonoid m => Comonoid (Colimit1 m)

instance Colimit2 -| Const2 where
  adj = dimap (\(Nat f) -> Nat $ Nat $ Const2 . f . Colimit2) $
               \ f -> Nat $ \ xs -> case xs of
                 Colimit2 fyx -> getConst2 $ runNat (runNat f) fyx

-- * Support for Tagged and Proxy

_Tagged :: Iso (Tagged s a) (Tagged t b) a b
_Tagged = dimap unTagged Tagged

instance Functor Proxy where
  fmap _ Proxy = Proxy

-- * Dictionaries

-- Dict :: Constraint -> * switches categories from the category of constraints to Hask
instance Functor Dict where
  fmap p Dict = Dict \\ p

-- * Lift

-- Lifting lets us define things an index up from simpler parts, recycle products, etc.

class Lift ~ s => Lifted (s :: (j -> k -> l) -> (i -> j) -> (i -> k) -> i -> l) | i j k l -> s where
  type Lift :: (j -> k -> l) -> (i -> j) -> (i -> k) -> i -> l
  _Lift :: Iso (s q f g a) (s r h e b) (q (f a) (g a)) (r (h b) (e b))

-- ** Lift1

newtype Lift1 p f g a = Lift { lower :: p (f a) (g a) }

instance Lifted Lift1 where
  type Lift = Lift1
  _Lift = dimap lower Lift

instance Functor Lift1 where
  fmap f = Nat $ Nat $ Nat $ _Lift $ runNat (runNat f)

instance Functor p => Functor (Lift1 p) where
  fmap f = Nat $ Nat $ _Lift $ first $ runNat f

instance Contravariant p => Contravariant (Lift1 p) where
  contramap f = Nat $ Nat $ _Lift $ lmap $ runNat f

instance Contravariant1 p => Contravariant1 (Lift1 p) where
  contramap1 (Nat f) = Nat $ _Lift (contramap1 f)

instance Functor1 p => Functor1 (Lift1 p) where
  fmap1 (Nat f) = Nat (_Lift $ fmap1 f)

instance Functor1 p => Functor (Lift1 p f) where
  fmap (Nat f) = Nat (_Lift $ fmap1 f)

instance (Functor p, Functor1 p, Functor f, Functor g) => Functor (Lift1 p f g) where
  fmap f = _Lift (bimap (fmap f) (fmap f))

-- ** LiftC

class r (p a) (q a) => LiftC r p q a
instance r (p a) (q a) => LiftC r p q a

instance Functor p => Functor (LiftC p) where
  fmap f = Nat $ Nat $ _Lift $ first $ runNat f

instance Contravariant p => Contravariant (LiftC p) where
  contramap f = Nat $ Nat $ _Lift $ lmap $ runNat f

instance Functor1 p => Functor1 (LiftC p) where
  fmap1 (Nat f) = Nat (_Lift $ fmap1 f)

instance Functor1 p => Functor (LiftC p e) where
  fmap = fmap1

instance Contravariant1 p => Contravariant1 (LiftC p) where
  contramap1 (Nat f) = Nat (_Lift $ contramap1 f)

instance Contravariant1 p => Contravariant (LiftC p e) where
  contramap = contramap1

instance Lifted LiftC where
  type Lift = LiftC
  _Lift = dimap (Sub Dict) (Sub Dict)

-- ** Lift2

newtype Lift2 p f g a b = Lift2 { lower2 :: p (f a) (g a) b }

instance Lifted Lift2 where
  type Lift = Lift2
  _Lift = dimap (Nat lower2) (Nat Lift2)

instance Functor Lift2 where
  fmap f = Nat $ Nat $ Nat $ _Lift $ runNat (runNat f)

instance Functor p => Functor (Lift2 p) where
  fmap f = Nat $ Nat $ _Lift $ first $ runNat f

instance Contravariant p => Contravariant (Lift2 p) where
  contramap f = Nat $ Nat $ _Lift $ lmap $ runNat f

instance Functor1 p => Functor (Lift2 p f) where
  fmap (Nat f) = Nat (_Lift $ fmap1 f)

instance Contravariant1 p => Contravariant (Lift2 p f) where
  contramap = contramap1

instance Functor1 p => Functor1 (Lift2 p) where
  fmap1 (Nat f) = Nat (_Lift $ fmap1 f)

-- * Functors

-- ** Products

-- *** Hask

instance Functor (,) where
  fmap f = Nat (Arrow.first f)

instance Functor1 (,) where
  fmap1 = Arrow.second

-- *** Constraint

infixr 2 &

-- needed because we can't partially apply (,) in the world of constraints
class (p, q) => p & q
instance (p, q) => p & q

-- Natural transformations form a category, using parametricity as a (stronger) proxy for naturality

instance Functor (&) where
  fmap f = Nat $ Sub $ Dict \\ f

instance Functor1 (&) where
  fmap1 p = Sub $ Dict \\ p

instance Functor ((&) p) where
  fmap = fmap1

-- ** Coproducts

-- *** Hask

instance Functor Either where
  fmap f = Nat (Arrow.left f)

-- ** Homs

-- *** Hask
instance Functor1 (->) where
  fmap1 = (.)

-- ** Constraint

instance Functor1 (:-) where
  fmap1 = dimap Hom runHom . fmap1

instance Contravariant (:-) where
  contramap f = Nat $ dimap Hom runHom (lmap f)

instance Category ((~>) :: x -> x -> *) => Functor1 (Hom :: x -> x -> *) where
  fmap1 g (Hom h) = Hom (g . h)

-- * Misc

instance Functor Tagged where
  fmap _ = Nat (_Tagged id)

instance Functor1 ((~>) :: j -> j -> *) => Functor1 (Nat :: (i -> j) -> (i -> j) -> *) where
  fmap1 (Nat ab) (Nat ca) = Nat (fmap1 ab ca)

instance Functor1 Either where
  fmap1 = Arrow.right

instance Functor1 At where
  fmap1 (Nat f) = _At f

instance Functor1 Tagged where
  fmap1 = Prelude.fmap

data Via (a :: x) (b :: x) (s :: x) (t :: x) where
  Via :: (s ~> a) -> (b ~> t) -> Via a b s t

instance Functor1 ((~>) :: x -> x -> *) => Functor (Via :: x -> x -> x -> x -> *) where
  fmap f = Nat $ Nat $ Nat $ \(Via sa bt) -> Via (fmap1 f sa) bt

instance Contravariant1 ((~>) :: x -> x -> *) => Contravariant (Via :: x -> x -> x -> x -> *) where
  contramap f = Nat $ Nat $ Nat $ \(Via sa bt) -> Via (contramap1 f sa) bt

instance Functor ((~>) :: x -> x -> *) => Functor (Via a :: x -> x -> x -> *) where
  fmap f = Nat $ Nat $ \(Via sa bt) -> Via sa (first f bt)

instance Contravariant ((~>) :: x -> x -> *) => Contravariant (Via a :: x -> x -> x -> *) where
  contramap f = Nat $ Nat $ \(Via sa bt) -> Via sa (lmap f bt)

instance Functor ((~>) :: x -> x -> *) => Functor (Via a b :: x -> x -> *) where
  fmap f = Nat $ \(Via sa bt) -> Via (first f sa) bt

instance Contravariant ((~>) :: x -> x -> *) => Contravariant (Via a b :: x -> x -> *) where
  contramap f = Nat $ \(Via sa bt) -> Via (lmap f sa) bt

instance Functor1 ((~>) :: x -> x -> *) => Functor (Via a b s :: x -> *) where
  fmap f (Via sa bt) = Via sa (fmap1 f bt)

instance Contravariant1 ((~>) :: x -> x -> *) => Contravariant (Via a b s :: x -> *) where
  contramap f (Via sa bt) = Via sa (contramap1 f bt)

instance Functor1 ((~>) :: x -> x -> *) => Functor1 (Via a b :: x -> x -> *) where
  fmap1 f (Via g h) = Via g (fmap1 f h)

instance Contravariant1 ((~>) :: x -> x -> *) => Contravariant1 (Via a b :: x -> x -> *) where
  contramap1 f (Via g h) = Via g (contramap1 f h)

-- |
-- @
-- get   = via _Get
-- unget = via _Unget
-- un    = via _Un
-- @
via :: forall (a :: *) (b :: *) (r :: *) (p :: x -> x -> *) (c :: x) (t :: *) (u :: *).
     Category p => (Via a b a b -> Via r (p c c) u t) -> (t -> u) -> r
via l m = case l (Via id id) of
  Via csa dbt -> csa $ m (dbt id)

mapping :: (Functor f, Category (Dom f)) => (Via a b a b -> Via a b s t) -> Iso (f s) (f t) (f a) (f b)
mapping l = case l (Via id id) of
  Via csa dbt -> dimap (fmap csa) (fmap dbt)

newtype Hom a b = Hom { runHom :: a ~> b }
_Hom = dimap runHom Hom

instance Contravariant (->) where
  contramap f = Nat (. f)

instance Category ((~>) :: x -> x -> *) => Contravariant (Hom :: x -> x -> *) where
  contramap f = Nat (_Hom (.f))

instance Contravariant ((~>)::j->j-> *) => Contravariant (Nat::(i->j)->(i->j)-> *) where
  contramap (Nat f) = Nat $ \g -> Nat $ lmap f $ runNat g

instance Contravariant Tagged where
  contramap _ = Nat (_Tagged id)

instance Contravariant1 Const1 where
  contramap1 _ = _Const id

instance Contravariant1 Const2 where
  contramap1 _ = _Const id

instance Contravariant1 p => Contravariant1 (Lift2 p) where
  contramap1 (Nat f) = Nat $ _Lift (contramap1 f)

instance Contravariant (Const1 k) where
  contramap _ = _Const id

instance Contravariant (Const2 k) where
  contramap _ = _Const id

type Ungetter t b = forall p. (Choice p, Functor p) => p b b -> p t t

unto :: (b ~> t) -> Ungetter t b
unto f = bimap f f

type Iso s t a b = forall p. Profunctor p => p a b -> p s t
type Iso' s a = Iso s s a a

bicontramap :: Bicontravariant p => (a ~> b) -> (c ~> d) -> p b d ~> p a c
bicontramap f g = lmap f . contramap1 g

type Getter s a = forall p. (Strong p, Contravariant1 p) => p a a -> p s s

to :: (s ~> a) -> Getter s a
to f = bicontramap f f

newtype Get r a b = Get { runGet :: a ~> r }
_Get = dimap runGet Get

instance Category ((~>)::i->i-> *) => Contravariant (Get (r :: i)) where
  contramap f = Nat (_Get (. f))

instance Contravariant1 (Get r) where
  contramap1 _ = _Get id

instance Functor1 (Get r) where
  fmap1 _ = _Get id

get :: (Category c, c ~ (~>)) => (Get a a a -> Get a s s) -> c s a
get l = runGet $ l (Get id)
-- get = via _Get

-- * Unget

newtype Unget r a b = Unget { runUnget :: r ~> b }
_Unget = dimap runUnget Unget

instance Functor (Unget r) where
  fmap _ = Nat $ _Unget id

instance Contravariant (Unget r) where
  contramap _ = Nat $ _Unget id

instance Category ((~>) :: i -> i -> *) => Functor1 (Unget (r :: i)) where
  fmap1 f = _Unget (f .)

unget :: (Category c, c ~ (~>)) => (Unget b b b -> Unget b t t) -> c b t
unget l = runUnget $ l (Unget id)
-- unget = via _Unget

(#) :: (Unget b b b -> Unget b t t) -> b -> t
(#) = unget

-- * Un

-- signatures needed to require endoprofunctors
newtype Un (p::i->i->j) (a::i) (b::i) (s::i) (t::i) = Un { runUn :: p t s ~> p b a }
_Un = dimap runUn Un

instance (Category ((~>)::j->j-> *), Functor1 p) => Contravariant (Un (p::i->i->j) a b) where
  contramap f = Nat $ _Un $ dimap Hom runHom $ lmap (fmap1 f)

instance (Category ((~>)::j->j-> *), Contravariant1 p) => Functor (Un (p::i->i->j) a b) where
  fmap f = Nat $ _Un $ dimap Hom runHom $ lmap (contramap1 f)

instance (Category ((~>)::j->j-> *), Contravariant p) => Functor1 (Un (p::i->i->j) a b) where
  fmap1 g = _Un $ dimap Hom runHom $ lmap (lmap g)

instance (Category ((~>)::j->j-> *), Functor p) => Contravariant1 (Un (p::i->i->j) a b) where
  contramap1 f = _Un $ dimap Hom runHom $ lmap (first f)

un :: (Un p a b a b -> Un p a b s t) -> p t s -> p b a
un l = runUn $ l (Un id)
-- un = via _Un

class (Contravariant p, Functor p) => PPhantom p
instance (Contravariant p, Functor p) => PPhantom p

class (Contravariant1 p, Functor1 p) => QPhantom p
instance (Contravariant1 p, Functor1 p) => QPhantom p

rmap :: Functor1 p => (a ~> b) -> p c a ~> p c b
rmap = fmap1

bimap :: Bifunctor (p::x->y->z) => (a ~> b) -> (c ~> d) -> p a c ~> p b d
bimap f g = first f . fmap1 g

dimap :: Profunctor (p::x->y->z) => (a ~> b) -> (c ~> d) -> p b c ~> p a d
dimap f g = lmap f . rmap g

-- tensor for a (skew) monoidal category
class Bifunctor p => Tensor (p :: x -> x -> x) where
  type Id p :: x
  associate :: p (p a b) c ~> p a (p b c)
  lambda    :: p (Id p) a ~> a
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

instance Tensor (&) where
  type Id (&) = (() :: Constraint)
  associate = Sub Dict
  lambda    = Sub Dict
  rho       = Sub Dict

associateLift :: (Profunctor (Cod2 p), Lifted s, Tensor p) => s p (s p f g) h ~> s p f (s p g h)
associateLift = Nat $ _Lift $ fmap1 (unget _Lift) . associate . first (get _Lift)

lambdaLift :: (Contravariant (Cod2 p), Constant k, Lifted s, Tensor p) => s p (k (Id p)) g ~> g
lambdaLift = Nat $ lmap (first (get _Const) . get _Lift) lambda

rhoLift :: (Functor1 (Cod2 p), Constant k, Lifted s, Tensor p) => Nat f (s p f (k (Id p)))
rhoLift = Nat $ rmap (unget _Lift . fmap1 (unget _Const)) rho

instance Tensor p => Tensor (Lift1 p) where
  type Id (Lift1 p) = Const1 (Id p)
  associate = associateLift
  lambda    = lambdaLift
  rho       = rhoLift

instance Tensor p => Tensor (Lift2 p) where
  type Id (Lift2 p) = Const2 (Id p)
  associate = associateLift
  lambda    = lambdaLift
  rho       = rhoLift

instance Tensor p => Tensor (LiftC p) where
  type Id (LiftC p) = ConstC (Id p)
  associate = associateLift
  lambda    = lambdaLift
  rho       = rhoLift

-- symmetric monoidal category
class Bifunctor p => Symmetric p where
  swap :: p a b ~> p b a

instance Symmetric (,) where
  swap (a,b) = (b, a)

instance Symmetric (&) where
  swap = Sub Dict

instance Symmetric Either where
  swap = either Right Left

instance Symmetric p => Symmetric (Lift1 p) where
  swap = Nat $ _Lift swap

instance Symmetric p => Symmetric (Lift2 p) where
  swap = Nat $ _Lift swap

instance Symmetric p => Symmetric (LiftC p) where
  swap = Nat $ _Lift swap

-- profunctor composition forms a weak category.
data Prof :: (j -> k -> *) -> (i -> j -> *) -> i -> k -> * where
  Prof :: p b c -> q a b -> Prof p q a c

associateProf :: Prof (Prof p q) r a c -> Prof p (Prof q r) a c
associateProf (Prof (Prof a b) c) = Prof a (Prof b c)

lambdaProf :: Functor1 p => Prof (~>) p a b -> p a b
lambdaProf (Prof h p) = rmap h p

rhoProf :: (Category q, q ~ (~>)) => p a b -> Prof p q a b
rhoProf p = Prof p id

class One ~ t => Terminal (t :: i) | i -> t where
  type One :: i
  terminal :: a ~> t

instance Terminal () where
  type One = ()
  terminal _ = ()

-- we can only offer the terminal object for Nat :: (i -> *), not Nat :: (i -> j)
instance Terminal (Const1 ()) where
  type One = Const1 ()
  terminal = Nat (Const . terminal)

instance Terminal (() :: Constraint) where
  type One = (() :: Constraint)
  terminal = Constraint.top

instance Terminal (ConstC ()) where
  type One = ConstC ()
  terminal = Nat (unget _Const . terminal)

class Zero ~ t => Initial (t :: i) | i -> t where
  type Zero :: i
  initial :: t ~> a

instance Initial Void where
  type Zero = Void
  initial = absurd

-- we can only offer the initial object for Nat :: (i -> *), not Nat :: (i -> j)
instance Initial (Const1 Void) where
  type Zero = Const1 Void
  initial = Nat $ initial . getConst

instance Initial (() ~ Bool) where
  type Zero = () ~ Bool
  initial = Constraint.bottom

instance Initial (ConstC (() ~ Bool)) where
  type Zero = ConstC (() ~ Bool)
  initial = Nat $ initial . get _Const

infixl 7 *
class (h ~ (~>), Symmetric ((*)::i->i->i), Tensor ((*)::i->i->i), Terminal (Id ((*)::i->i->i))) => Cartesian (h :: i -> i -> *) | i -> h where
  type (*) :: i -> i -> i
  fst   :: forall (a::i) (b::i). a * b ~> a
  snd   :: forall (a::i) (b::i). a * b ~> b
  (&&&) :: forall (a::i) (b::i) (c::i). (a ~> b) -> (a ~> c) -> a ~> b * c

instance Cartesian (->) where
  type (*) = (,)
  fst   = Prelude.fst
  snd   = Prelude.snd
  (&&&) = (Arrow.&&&)

instance Cartesian (:-) where
  type (*) = (&)
  fst = Sub Dict
  snd = Sub Dict
  p &&& q = Sub $ Dict \\ p \\ q

instance Cartesian (Nat :: (i -> *) -> (i -> *) -> *) where
  type (*) = Lift (,)
  fst = Nat $ fst . lower
  snd = Nat $ snd . lower
  Nat f &&& Nat g = Nat $ Lift . (f &&& g)

instance Cartesian (Nat :: (i -> Constraint) -> (i -> Constraint) -> *) where
  type (*) = LiftC (&)
  fst = Nat $ fst . get _Lift
  snd = Nat $ snd . get _Lift
  Nat f &&& Nat g = Nat $ unget _Lift . (f &&& g)

-- todo make this a pre-req to Tensor?
class (Cartesian ((~>) :: i -> i -> *), Profunctor p) => Strong (p :: i -> i -> *) where
  {-# MINIMAL _1 | _2 #-}
  _1 :: p a b -> p (a * c) (b * c)
  _1 = dimap swap swap . _2
  _2 :: p a b -> p (c * a) (c * b)
  _2 = dimap swap swap . _1

instance Strong (->) where
  _1 = first
  _2 = fmap1

instance Strong (:-) where
  _1 = first
  _2 = fmap1

instance Strong (Nat::(i-> *)->(i-> *)-> *) where
  _1 = first
  _2 = fmap1

instance Strong (Nat::(i-> Constraint)->(i-> Constraint)-> *) where
  _1 = first
  _2 = fmap1

instance Cartesian ((~>)::i->i-> *) => Strong (Get (r::i)) where
  _1 = _Get (. fst)

type Lens s t a b = forall p. Strong p => p a b -> p s t

infixl 6 +
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

instance Cocartesian (Nat :: (i -> *) -> (i -> *) -> *) where
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

instance Choice (Nat :: (i -> *) -> (i -> *) -> *) where
  _Left (Nat f) = Nat $ _Lift (_Left f)
  _Right (Nat g) = Nat $ _Lift (_Right g)

instance Choice Tagged where
  _Left  = bimap inl inl
  _Right = bimap inr inr

instance Cocartesian ((~>) :: i -> i -> *) => Choice (Unget (r :: i)) where
  _Left = bimap inl inl
  _Right = bimap inr inr

type Prism s t a b = forall p. Choice p => p a b -> p s t

type a ^ b = Exp b a
infixr 8 ^

class (Profunctor (Exp :: x -> x -> x), Cartesian k) => CCC (k :: x -> x -> *) | x -> k where
  type Exp :: x -> x -> x
  curried :: forall (a :: x) (b :: x) (c :: x) (a' :: x) (b' :: x) (c' :: x). Iso (a * b ~> c) (a' * b' ~> c') (a ~> c^b) (a' ~> c'^b')

instance CCC (->) where
  type Exp = (->)
  curried = dimap Prelude.curry Prelude.uncurry

instance CCC (Nat :: (i -> *) -> (i -> *) -> *) where
  type Exp = Lift (->)
  curried = dimap hither yon where
    hither (Nat f) = Nat $ \a -> Lift $ \b -> f (Lift (a, b))
    yon (Nat f) = Nat $ \(Lift (a,b)) -> lower (f a) b

curry :: forall (a :: i) (b :: i) (c :: i). CCC ((~>)::i -> i -> *) => ((a * b) ~> c) -> a ~> c^b
curry = get curried

uncurry :: forall (a :: i) (b :: i) (c :: i). CCC ((~>)::i -> i -> *) => (a ~> c^b) -> (a * b) ~> c
uncurry = unget curried

apply :: forall (a :: i) (b :: i). CCC ((~>)::i -> i -> *) => b^a * a ~> b
apply = uncurry id

unapply :: forall (a :: i) (b :: i). CCC ((~>) :: i -> i -> *) => a ~> (a * b)^b
unapply = curry id

cccAdj :: forall (e :: i). CCC ((~>) :: i -> i -> *) => (*) e -: Exp e
cccAdj = dimap (. swap) (. swap) . curried

instance (,) e -| (->) e where
  adj = cccAdj

instance Lift1 (,) e -| Lift1 (->) e where
  adj = cccAdj

-- monoidal functors preserve the structure of our tensor and take monoid objects to monoid objects

class (Cartesian ((~>) :: x -> x -> *), Cartesian ((~>) :: y -> y -> *), Functor f) => Monoidal (f :: x -> y) where
  ap0 :: One ~> f One
  ap2 :: f a * f b ~> f (a * b)

instance Monoidal Dict where
  ap0 () = Dict
  ap2 (Dict, Dict) = Dict

instance Monoid m => Monoid (Dict m) where
  one = oneM
  mult = multM

-- lift applicatives for Hask
instance Applicative.Applicative f => Monoidal f where
  ap0 = Applicative.pure
  ap2 = uncurry $ Applicative.liftA2 (,)

instance Monoidal ((:-) f) where
  ap0 () = terminal
  ap2 = uncurry (&&&)

instance Monoid m => Monoid (f :- m) where
  one = oneM
  mult = multM

instance Monoidal (Lift1 (->) f) where
  ap0 = curry fst
  ap2 = get zipR

instance Monoid m => Monoid (Lift1 (->) f m) where
  one = oneM
  mult = multM

--instance Monoidal (LiftC (|-) f) where
--  ap0 = curry fst
--  ap2 = get zipR

instance Monoidal ((|-) f) where
  ap0 = curry fst
  ap2 = get zipR

instance Monoid m => Monoid (f |- m) where
  one = oneM
  mult = multM

instance Monoidal (Nat f :: (i -> *) -> *) where
  ap0 () = terminal
  ap2 = uncurry (&&&)

instance Monoidal (Nat f :: (i -> Constraint) -> *) where
  ap0 () = terminal
  ap2 = uncurry (&&&)

instance (Monoidal (Nat f), Monoid m) => Monoid (Nat f m) where
  one = oneM
  mult = multM

-- inherited from base
-- instance Monoidal (Tagged s)
-- instance Monoid m => Monoid (Tagged s m)

instance Cartesian ((~>) :: i -> i -> *) => Monoidal (Proxy :: i -> *) where
  ap0 () = Proxy
  ap2 (Proxy, Proxy) = Proxy

-- instance Monoid (Proxy a) -- from base

-- * Monads over our kind-indexed categories

class Monoidal (m :: x -> x) => Monad (m :: x -> x) where
  join :: m (m a) ~> m a

instance (Applicative.Applicative m, Prelude.Monad m) => Monad m where
  join = Monad.join

-- * Opmonoidal functors between cocartesian categories

class (Cocartesian ((~>) :: x -> x -> *), Cocartesian ((~>) :: y -> y -> *), Functor f) => Opmonoidal (f :: x -> y) where
  op0 :: f Zero ~> Zero
  op2 :: f (a + b) ~> f a + f b

instance Opmonoidal ((,) e) where
  op0 = snd
  op2 (e,ab) = bimap ((,) e) ((,) e) ab

instance Comonoid m => Comonoid (e, m) where
  zero = zeroOp
  comult = comultOp

instance Opmonoidal Identity where
  op0 = runIdentity
  op2 = bimap Identity Identity . runIdentity

instance Comonoid m => Comonoid (Identity m) where
  zero = zeroOp
  comult = comultOp

instance Opmonoidal (At x) where
  op0 (At (Const x)) = x
  op2 (At (Lift eab))= bimap At At eab

instance Comonoid m => Comonoid (At x m) where
  zero = zeroOp
  comult = comultOp

instance Opmonoidal (Lift1 (,) e) where
  op0 = snd
  op2 = Nat $ Lift . bimap Lift Lift . op2 . fmap lower . lower

instance Comonoid m => Comonoid (Lift1 (,) e m) where
  zero = zeroOp
  comult = comultOp

instance Opmonoidal (Tagged s) where
  op0 = unTagged
  op2 = bimap Tagged Tagged . unTagged

instance Comonoid m => Comonoid (Tagged s m) where
  zero = zeroOp
  comult = comultOp

-- * An = Identity for Nat (i -> *)
newtype An (f :: i -> *) (a :: i) = An { runAn :: f a }
_An = dimap runAn An

instance Functor f => Functor (An f) where
  fmap = _An . fmap

instance Comonad An where
  extract = Nat runAn
  duplicate = Nat An

instance Monoidal An where
  ap0 = Nat An
  ap2 = Nat $ \(Lift (An x, An y)) -> An (Lift (x, y))

instance Monoid m => Monoid (An m) where
  one = oneM
  mult = multM

instance Monad An where
  join = Nat runAn

instance Opmonoidal An where
  op0 = Nat runAn
  op2 = Nat $ \(An (Lift ea)) -> Lift (bimap An An ea)

instance Comonoid m => Comonoid (An m) where
  zero = zeroOp
  comult = comultOp

instance An -| An where
  adj = dimap (dimap (Nat An) (Nat An)) (dimap (Nat runAn) (Nat runAn))

instance Contravariant f => Contravariant (An f) where
  contramap = _An . contramap

instance Functor An where
  fmap (Nat f) = Nat $ _An f

instance Monoidal f => Monoidal (An f) where
  ap0 = An . ap0
  ap2 = An . ap2 . bimap runAn runAn

instance Opmonoidal f => Opmonoidal (An f) where
  op0 = op0 . runAn
  op2 = bimap An An . op2 . runAn

instance (Opmonoidal f, Comonoid m) => Comonoid (An f m) where
  zero = zeroOp
  comult = comultOp

-- a monoid object in a cartesian category
class Cartesian ((~>) :: i -> i -> *) => Monoid (m :: i) where
  one  :: One ~> m
  mult :: m * m ~> m

-- monoidal functors take monoids to monoids

oneM :: (Monoidal u, Monoid m) => One ~> u m
oneM = fmap one . ap0

multM :: (Monoidal u, Monoid m) => u m * u m ~> u m
multM = fmap mult . ap2

instance Monoid.Monoid m => Monoid m where
  one () = Monoid.mempty
  mult = uncurry Monoid.mappend

instance Monoid (Const1 ()) where
  one = id
  mult = lambda

instance Monoid (() :: Constraint) where
  one = id
  mult = lambda

mappend :: (Monoid m, CCC (Arr m)) => m ~> m^m
mappend = curry mult

class Cocartesian (Arr m) => Comonoid m where
  zero   :: m ~> Zero
  comult :: m ~> m + m

-- opmonoidal functors take comonoids to comonoids

zeroOp :: (Opmonoidal f, Comonoid m) => f m ~> Zero
zeroOp = op0 . fmap zero

comultOp :: (Opmonoidal f, Comonoid m) => f m ~> f m + f m
comultOp = op2 . fmap comult

instance Comonoid Void where
  zero = id
  comult = absurd

instance Comonoid (Const1 Void) where
  zero = id
  comult = Nat $ absurd . getConst

-- instance Comonoid (Const2 (Const1 Void)) where

class Functor f => Strength f where
  strength :: a * f b ~> f (a * b)

instance Prelude.Functor f => Strength f where
  strength (a,fb) = fmap ((,) a) fb

-- proposition: all right adjoints on Cartesian categories should be strong
-- strengthR   :: (f -| u, Cartesian (Dom u)) => a * u b ~> u (a * b)
-- costrengthL :: (f -| u, Cartesian (Dom f)) => f (a + b) ~> a + f b

-- what is usually called 'costrength' is more like a 'left strength' or a 'right strength'
-- repurposing this term for a real 'co'-strength
class Functor f => Costrength (f :: x -> x) where
  costrength :: f (a + b) ~> a + f b

instance Traversable.Traversable f => Costrength f where
  costrength = Traversable.sequence

ap :: (Monoidal f, CCC (Dom f), CCC (Cod f)) => f (b ^ a) ~> f b ^ f a
ap = curry (fmap apply . ap2)

return :: (Monoidal f, Strength f, CCC (Dom f)) => a ~> f a
return = fmap (lambda . swap) . strength . fmap1 ap0 . rho

class (Functor f, Category (Dom f)) => Comonad (f :: x -> x) where
  {-# MINIMAL extract, (duplicate | extend) #-}
  duplicate :: f a ~> f (f a)
  duplicate = extend id
  extend :: (f a ~> b) -> f a ~> f b
  extend f = fmap f . duplicate
  extract   :: f a ~> a

instance Comonad.Comonad f => Comonad f where
  duplicate = Comonad.duplicate
  extend = Comonad.extend
  extract = Comonad.extract

-- indexed store
data Store s a i = Store (s ~> a) (s i)

instance Functor (Store s) where
  fmap f = Nat $ \(Store g s) -> Store (f . g) s

instance Comonad (Store s) where
  extract = Nat $ \(Store f s) -> runNat f s
  duplicate = Nat $ \(Store f s) -> Store (Nat $ Store f) s

-- The dual of Conor McBride's "Atkey" adapted to this formalism
--
-- Cokey i :: Hask -> Nat
-- Cokey :: x -> * -> x -> *
newtype Cokey i a j = Cokey { runCokey :: (i ~ j) => a }

instance Functor (Cokey i) where
  fmap f = Nat $ \xs -> Cokey $ f (runCokey xs)

instance Functor1 Cokey where
  fmap1 f = Nat $ \xs -> Cokey $ f (runCokey xs)

instance Monoidal (Cokey i) where
  ap0 = Nat $ \a -> Cokey (getConst a)
  ap2 = Nat $ \ab -> Cokey $ case ab of
    Lift (Cokey a, Cokey b) -> (a, b)

instance Monoid m => Monoid (Cokey i m) where
  one = oneM
  mult = multM

-- Conor McBride's "Atkey" adapted to this formalism
--
-- Key i :: Hask -> Nat
-- Key :: x -> * -> x -> *
data Key i a j where
  Key :: a -> Key i a i

instance Functor (Key i) where
  fmap f = Nat $ \ (Key a) -> Key (f a)

instance Functor1 Key where
  fmap1 f = Nat $ \ (Key a) -> Key (f a)

instance Opmonoidal (Key i) where
  op0 = Nat $ \(Key v) -> Const v
  op2 = Nat $ \(Key eab) -> Lift (bimap Key Key eab)

instance Comonoid m => Comonoid (Key i m) where
  zero = zeroOp
  comult = comultOp

-- * Traditional product categories w/ adjoined identities
data Prod :: (i,j) -> (i,j) -> * where
  Want :: Prod a a
  Have :: (a ~> b) -> (c ~> d) -> Prod '(a,c) '(b,d)

instance (Category ((~>) :: i -> i -> *), Category ((~>) :: j -> j -> *)) => Category (Prod :: (i,j) -> (i,j) -> *) where
  id = Want
  Want . f = f
  f . Want = f
  Have f f' . Have g g' = Have (f . g) (f' . g')

instance (Functor ((~>) a :: i -> *), Functor ((~>) b :: j -> *)) => Functor (Prod '(a,b) :: (i, j) -> *) where
  fmap Want f = f
  fmap f Want = f
  fmap (Have f g) (Have f' g') = Have (fmap f f') (fmap g g')

instance (Functor1 ((~>) :: i -> i -> *), Functor1 ((~>) :: j -> j -> *)) => Functor1 (Prod :: (i, j) -> (i, j) -> *) where
  fmap1 Want f = f
  fmap1 f Want = f
  fmap1 (Have f g) (Have f' g') = Have (fmap1 f f') (fmap1 g g')

instance (Contravariant ((~>) :: i -> i -> *), Contravariant ((~>) :: j -> j -> *)) => Contravariant (Prod :: (i, j) -> (i, j) -> *) where
  contramap Want = id
  contramap (Have f g) = Nat $ \case
    Want       -> Have f g
    Have f' g' -> Have (lmap f f') (lmap g g')

runProd :: forall (a :: i) (b :: i) (c :: j) (d :: j).
           (Category ((~>) :: i -> i -> *), Category ((~>) :: j -> j -> *))
        => Prod '(a,c) '(b,d) -> (a ~> b, c ~> d)
runProd Want       = (id,id)
runProd (Have f g) = (f,g)

runFst
  :: forall (a :: i) (b :: i) (c :: j) (d :: j).
     Category ((~>) :: i -> i -> *)
  => Prod '(a,c) '(b,d) -> a ~> b
runFst Want       = id
runFst (Have f _) = f

runSnd
  :: forall (a :: i) (b :: i) (c :: j) (d :: j).
     Category ((~>) :: j -> j -> *)
  => Prod '(a,c) '(b,d) -> c ~> d
runSnd Want       = id
runSnd (Have _ g) = g

-- Δ -| (*)
diagProdAdj :: forall (a :: i) (b :: i) (c :: i) (a' :: i) (b' :: i) (c' :: i).
   Cartesian ((~>) :: i -> i -> *) =>
   Iso ('(a,a) ~> '(b,c)) ('(a',a') ~> '(b',c')) (a ~> b * c) (a' ~> b' * c')
diagProdAdj = dimap (uncurry (&&&) . runProd) $ \f -> Have (fst . f) (snd . f)

-- (+) -| Δ
sumDiagAdj :: forall (a :: i) (b :: i) (c :: i) (a' :: i) (b' :: i) (c' :: i).
   Cocartesian ((~>) :: i -> i -> *) =>
   Iso (b + c ~> a) (b' + c' ~> a') ('(b,c) ~> '(a,a)) ('(b',c') ~> '(a',a'))
sumDiagAdj = dimap (\f -> Have (f . inl) (f . inr)) (uncurry (|||) . runProd)

-- * Work in progress

-- | Copower e -| (~>) e
type family Copower :: x -> * -> x

type instance Copower = (,)
type instance Copower = Copower1
type instance Copower = Copower2

-- Nat :: (i -> *) -> (i -> *) -> * is tensored. (Copowered over Hask)
data Copower1 f x a = Copower (f a) x

instance Functor Copower1 where
  fmap (Nat f) = Nat $ Nat $ \(Copower fa x) -> Copower (f fa) x

instance Functor1 Copower1 where
  fmap1 f = Nat $ \(Copower fa x) -> Copower fa (f x)

instance Functor (Copower1 f) where
  fmap = fmap1

instance Functor f => Functor1 (Copower1 f) where
  fmap1 f (Copower fa x) = Copower (fmap f fa) x

instance Functor f => Functor  (Copower1 f a) where
  fmap = fmap1

instance Contravariant f => Contravariant1 (Copower1 f) where
  contramap1 f (Copower fa x) = Copower (contramap f fa) x

instance Contravariant f => Contravariant  (Copower1 f a) where
  contramap = contramap1

instance Copower1 e -| Nat e where
  adj = dimap (\(Nat f) a -> Nat $ \e -> f (Copower e a))
              (\f -> Nat $ \(Copower e a) -> runNat (f a) e)

-- Nat :: (i -> j -> *) -> (i -> j -> *) -> * is tensored. (Copowered over Hask)
data Copower2 f x a b = Copower2 (f a b) x

instance Functor Copower2 where
  fmap f = Nat $ Nat $ Nat $ \(Copower2 fab x) -> Copower2 (runNat (runNat f) fab) x

instance Functor1 Copower2 where
  fmap1 f = Nat $ Nat $ \(Copower2 fab x) -> Copower2 fab (f x)

instance Functor (Copower2 f) where
  fmap = fmap1

instance Functor f => Functor1 (Copower2 f) where
  fmap1 f = Nat $ \(Copower2 fa x) -> Copower2 (first f fa) x

instance Functor f => Functor (Copower2 f x) where
  fmap = fmap1

instance Contravariant f => Contravariant1 (Copower2 f) where
  contramap1 f = Nat $ \(Copower2 fa x) -> Copower2 (lmap f fa) x

instance Contravariant f => Contravariant (Copower2 f x) where
  contramap = contramap1

instance Functor1 f => Functor1 (Copower2 f x) where
  fmap1 f (Copower2 fab x) = Copower2 (fmap1 f fab) x

instance Functor1 f => Functor (Copower2 f x a) where
  fmap = fmap1

instance Contravariant1 f => Contravariant1 (Copower2 f x) where
  contramap1 f (Copower2 fab x) = Copower2 (contramap1 f fab) x

instance Contravariant1 f => Contravariant (Copower2 f x a) where
  contramap = contramap1

instance Copower2 e -| Nat e where
  adj = dimap (\f a -> Nat $ Nat $ \e -> runNat (runNat f) (Copower2 e a))
              (\f -> Nat $ Nat $ \(Copower2 e a) -> runNat (runNat (f a)) e)
              -- Nat $ \(Copower2 e a) -> runNat (runNat (f a)) e)

-- we now need the notion of an adjunction to a dual category for handling powers

-- | the isomorphism that witnesses f -| u
type (f :: y -> x) =: (u :: x -> y) = forall a b a' b'. Iso (b ~> f a) (b' ~> f a') (a ~> u b) (a' ~> u b')

class (Profunctor (Power :: * -> j -> j), k ~ (~>)) => Powered (k :: j -> j -> *) | j -> k where
  type Power :: * -> j -> j
  _Power :: forall (u :: *) (u' :: *) (a :: j) (a' :: j) (b :: j) (b' :: j).
             Iso (a ~> Power u b) (a' ~> Power u' b') (u -> (a ~> b)) (u' -> (a' ~> b'))

instance Powered (->) where
  type Power = (->)
  _Power = dimap Prelude.flip Prelude.flip

newtype Power1 v f a = Power { runPower :: v -> f a }

instance Contravariant Power1 where
  contramap f = Nat $ Nat $ Power . lmap f . runPower

instance Functor1 Power1 where
  fmap1 f = Nat $ Power . fmap1 (runNat f) . runPower

instance Functor (Power1 v) where
  fmap = fmap1

instance Functor f => Functor (Power1 v f) where
  fmap f = Power . fmap1 (fmap f) . runPower

-- Nat :: (i -> *) is powered over Hask
instance Powered (Nat :: (i -> *) -> (i -> *) -> *) where
  type Power = Power1
  _Power = dimap
     (\k v -> Nat $ \f -> runPower (runNat k f) v)
     (\k -> Nat $ \a' -> Power $ \u' -> runNat (k u') a')

-- * Kan extensions

type family Ran :: (i -> j) -> (i -> k) -> j -> k
type family Lan :: (i -> j) -> (i -> k) -> j -> k

type instance Ran = Ran1

newtype Ran1 f g a = Ran { runRan :: forall r. f r^a ~> g r }

-- instance Category ((~>) :: j -> j -> *) => Contravariant (Ran1 :: (i -> j) -> (i -> *) -> j -> *) where
--   contramap (Nat f) = Nat $ Nat $ \(Ran k) -> Ran $ k . fmap1 f

instance Category (Cod f) => Functor (Ran1 f) where
  fmap (Nat f) = Nat $ \(Ran k) -> Ran $ f . k

type instance Lan = Lan1 -- :: (i -> j) -> (i -> *) -> j -> *
data Lan1 f g a where
  Lan :: (f b ~> a) -> g b -> Lan1 f g a

-- newtype Codensity f a = Codensity { runCodensity :: forall r. f r^a ~> f r }
-- newtype Yoneda f a = Yoneda { runYoneda :: forall r. r^a ~> f r }

-- * Work in progress

-- constraints can be upgraded to a cartesian closed category with reflection

infixr 9 |-
-- exponentials for constraints
class p |- q where
  implies :: p :- q

-- convert to and from the internal representation of dictionaries

_Sub :: Iso (p :- q) (p' :- q') (Dict p -> Dict q) (Dict p' -> Dict q')
_Sub = dimap (\pq Dict -> case pq of Sub q -> q) (\f -> Sub $ f Dict)

newtype Magic p q r = Magic ((p |- q) => r)

reify :: forall p q r. ((p |- q) => r) -> (p :- q) -> r
reify k = unsafeCoerce (Magic k :: Magic p q r)

_Implies :: Iso (p :- q) (p' :- q') (Dict (p |- q)) (Dict (p' |- q'))
_Implies = dimap (reify Dict) (\Dict -> implies)

instance Contravariant (|-) where
  contramap f = Nat $ unget _Sub $ un _Implies (. f)

instance Functor1 (|-) where
  fmap1 f = unget _Sub $ un _Implies (f .)

instance Functor ((|-) p) where
  fmap = fmap1

applyConstraint :: forall p q. (p |- q & p) :- q
applyConstraint = Sub $ Dict \\ (implies :: p :- q)

unapplyConstraint :: p :- q |- (p & q)
unapplyConstraint = Sub $ get _Implies (Sub Dict)

instance (&) p -| (|-) p where
  adj = cccAdj

instance CCC (:-) where
  type Exp = (|-)
  curried = dimap (\q -> fmap q . unapplyConstraint) (\p -> applyConstraint . first p)

data Unit (a :: ()) (b :: ()) = Unit

instance Category Unit where
  id = Unit
  Unit . Unit = Unit

instance Groupoid Unit where
  inverse Unit = Unit

instance Symmetric Unit where
  swap = inverse

instance Contravariant Unit where
  contramap f = Nat (. f)

instance Functor Unit where
  fmap = contramap . inverse

instance Functor1 Unit where
  fmap1 = (.)

instance Contravariant1 Unit where
  contramap1 = fmap1 . inverse

instance Functor (Unit a) where
  fmap = fmap1

instance Contravariant (Unit a) where
  contramap = contramap1

instance Terminal '() where
  type One = '()
  terminal = Unit

instance Initial '() where
  type Zero = '()
  initial = Unit

class Category c => Groupoid c where
  inverse :: c a b -> c b a

instance (Groupoid ((~>) :: i -> i -> *), Groupoid ((~>) :: j -> j -> *)) =>
  Groupoid (Prod :: (i, j) -> (i, j) -> *) where
  inverse Want = Want
  inverse (Have f g) = Have (inverse f) (inverse g)

data Empty (a :: Void) (b :: Void) = Empty (Empty a b)

instance Category Empty where
  id = Empty id
  (.) f = f `Prelude.seq` spin f where spin (Empty f) = spin f

instance Groupoid Empty where
  inverse !f = Empty (inverse f)

instance Symmetric Empty where
  swap = inverse

instance Contravariant Empty where
  contramap f = Nat (. f)

instance Functor Empty where
  fmap = contramap . inverse

instance Functor1 Empty where
  fmap1 = (.)

instance Contravariant1 Empty where
  contramap1 = fmap1 . inverse

instance Functor (Empty a) where
  fmap = fmap1

instance Contravariant (Empty a) where
  contramap = contramap1

-- No :: Void -> *
data No (a :: Void)

instance Functor No where
  fmap !f = Prelude.undefined
