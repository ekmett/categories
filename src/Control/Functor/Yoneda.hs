-------------------------------------------------------------------------------------------
-- |
-- Module	: Control.Functor.Yoneda
-- Copyright 	: 2008 Edward Kmett
-- License	: BSD
--
-- Maintainer	: Edward Kmett <ekmett@gmail.com>
-- Stability	: experimental
-- Portability	: portable
--
-- The Yoneda lemma can be realized as the Kan extension along Identity
-- However, having this special instance allows us to define Yoneda f as a monad, 
-- comonad, etc. based on whatever properties the base functor has, without
-- limiting ourselves to what Ran f f can manage.
--
-- Performance wise, Yoneda may make your monad more efficient at handling a bunch of 
-- fmaps, while CoYoneda may do the same for a comonad assuming you require a greater than
-- linear amount of time to fmap over your structure. You can apply each in either role
-- but the asymptotics will probably not be in your favor.
--
-------------------------------------------------------------------------------------------

module Control.Functor.Yoneda
	( Yoneda(Yoneda,runYoneda), ranToYoneda, yonedaToRan, lowerYoneda
	, CoYoneda(CoYoneda), lanToCoYoneda, coYonedaToLan, liftCoYoneda
	) where

import Control.Applicative
import Control.Comonad.HigherOrder
import Control.Comonad.Cofree
import Control.Comonad.Context
import Control.Comonad.Reader
import Control.Comonad.Trans
import Control.Functor.Extras
import Control.Functor.KanExtension
import Control.Functor.Pointed
import Control.Functor.HigherOrder
import Control.Monad.Identity
import Control.Monad.HigherOrder
import Control.Monad.Free
import Control.Monad.Trans
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class

-- Yoneda ~ Ran Identity
newtype Yoneda f a = Yoneda { runYoneda :: forall b. ((a -> b) -> f b) } 

ranToYoneda :: Ran Identity f :~> Yoneda f
ranToYoneda r = Yoneda (\f -> runRan r (Identity . f))

yonedaToRan :: Yoneda f :~> Ran Identity f
yonedaToRan y = Ran (\f -> runYoneda y (runIdentity . f))

lowerYoneda :: Yoneda f :~> f 
lowerYoneda m = runYoneda m id

instance Functor (Yoneda f) where
	fmap f m = Yoneda (\k -> runYoneda m (k . f))

instance Pointed f => Pointed (Yoneda f) where
	point a = Yoneda (\f -> point (f a))

instance Applicative f => Applicative (Yoneda f) where
	pure a = Yoneda (\f -> pure (f a))
	m <*> n = Yoneda (\f -> runYoneda m (f .) <*> runYoneda n id)

instance Monad f => Monad (Yoneda f) where
	return a = Yoneda (\f -> return (f a))
	m >>= k = Yoneda (\f -> runYoneda m id >>= \a -> runYoneda (k a) f)

instance HFunctor Yoneda where
	ffmap = fmap
	hfmap f y = Yoneda (f . runYoneda y)

-- f a -> Yoneda f a 
instance HPointed Yoneda where
	hreturn a = Yoneda (\f -> fmap f a) 

-- exists because Monad doesn't require Functor!
instance MonadTrans Yoneda where
	lift a = Yoneda (\f -> liftM f a)

instance ComonadTrans Yoneda where
	colift = hreturn

-- Yoneda f a -> f a
instance HCopointed Yoneda where
	hextract t = runYoneda t id

instance HMonad Yoneda where
	hbind f = f . hextract 

instance HComonad Yoneda where
	hextend f = hreturn . f

instance Copointed f => Copointed (Yoneda f) where
	extract = extract . hextract

instance Comonad f => Comonad (Yoneda f) where
	extend k m = Yoneda (\f -> extend (f . k . hreturn) (hextract m))

instance MonadState e m => MonadState e (Yoneda m) where
	get = lift get
	put = lift . put

instance MonadReader e m => MonadReader e (Yoneda m) where
	ask = lift ask
	local r = lift . local r . lowerYoneda

instance MonadWriter e m => MonadWriter e (Yoneda m) where
	tell = lift . tell
	listen = lift . listen . flip runYoneda id 
	pass = lift . pass . lowerYoneda

instance MonadFree f m => MonadFree f (Yoneda m) where
	inFree = lift . inFree . fmap lowerYoneda

instance RunMonadFree f m => RunMonadFree f (Yoneda m) where
	cataFree l r = cataFree l r . lowerYoneda

instance ComonadCofree f m => ComonadCofree f (Yoneda m) where
	outCofree = fmap colift . outCofree . lowerYoneda

instance RunComonadCofree f m => RunComonadCofree f (Yoneda m) where
	anaCofree l r = colift . anaCofree l r

instance ComonadContext e m => ComonadContext e (Yoneda m) where
	getC = getC . lowerYoneda
	modifyC s = modifyC s . lowerYoneda

instance ComonadReader e m => ComonadReader e (Yoneda m) where
	askC = askC . lowerYoneda
	

-- | Left Kan Extensions
-- CoYoneda ~ Lan Identity
data CoYoneda f a = forall b. CoYoneda (b -> a) (f b)

lanToCoYoneda :: Lan Identity f :~> CoYoneda f 
lanToCoYoneda (Lan f v) = CoYoneda (f . Identity) v

coYonedaToLan :: CoYoneda f :~> Lan Identity f
coYonedaToLan (CoYoneda f v) = Lan (f . runIdentity) v

instance Functor (CoYoneda f) where
	fmap f (CoYoneda g v) = CoYoneda (f . g) v

instance Pointed f => Pointed (CoYoneda f) where
	point = hreturn . point

instance Applicative f => Applicative (CoYoneda f) where
	pure = hreturn . pure
	m <*> n = CoYoneda id (hextract m <*> hextract n)

instance Monad m => Monad (CoYoneda m) where
	return = CoYoneda id . return
	CoYoneda f v >>= k = CoYoneda id (v >>= (\(CoYoneda f' v') -> liftM f' v') . k . f)

instance HFunctor CoYoneda where
	ffmap = fmap 
	hfmap f (CoYoneda g v) = CoYoneda g (f v)

instance HPointed CoYoneda where
	hreturn = CoYoneda id

instance HMonad CoYoneda where
	hbind f = f . hextract

instance HComonad CoYoneda where
	hextend f = hreturn . f

instance HCopointed CoYoneda where
	hextract (CoYoneda f v) = fmap f v

liftCoYoneda :: f :~> CoYoneda f
liftCoYoneda = CoYoneda id

-- | Just a conceptual nicety for monads since they aren't functors in Haskell. this is otherwise just hextract
lowerCoYoneda :: Monad f => CoYoneda f :~> f 
lowerCoYoneda (CoYoneda f v) = liftM f v 

instance Copointed w => Copointed (CoYoneda w) where
	extract (CoYoneda f v) = f (extract v)

instance Comonad w => Comonad (CoYoneda w) where
	extend k (CoYoneda f v) = CoYoneda id $ extend (k . CoYoneda f) v

instance MonadTrans CoYoneda where
	lift = CoYoneda id

instance ComonadTrans CoYoneda where
	colift = CoYoneda id

-- All the (Co)monadFoo CoYoneda instances

instance ComonadCofree f m => ComonadCofree f (CoYoneda m) where
	outCofree = fmap colift . outCofree . hextract

instance RunComonadCofree f m => RunComonadCofree f (CoYoneda m) where
	anaCofree l r = colift . anaCofree l r

instance ComonadContext e m => ComonadContext e (CoYoneda m) where
	getC = getC . hextract
	modifyC s = modifyC s . hextract

instance ComonadReader e m => ComonadReader e (CoYoneda m) where
	askC = askC . hextract
	
instance MonadState e m => MonadState e (CoYoneda m) where
	get = lift get
	put = lift . put

instance MonadReader e m => MonadReader e (CoYoneda m) where
	ask = lift ask
	local r = lift . local r . lowerCoYoneda

instance MonadWriter e m => MonadWriter e (CoYoneda m) where
	tell = lift . tell
	listen = lift . listen . lowerCoYoneda
	pass = lift . pass . lowerCoYoneda

instance MonadFree f m => MonadFree f (CoYoneda m) where
	inFree = lift . inFree . fmap lowerCoYoneda

instance RunMonadFree f m => RunMonadFree f (CoYoneda m) where
	cataFree l r = cataFree l r . lowerCoYoneda
