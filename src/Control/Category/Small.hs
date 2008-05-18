{-# OPTIONS_GHC -fglasgow-exts #-}
module Small where

import Prelude hiding (curry,uncurry)

class CCC k where
        type Exp k :: * -> * -> *
	type Prod k :: * -> * -> *
        apply :: k (Prod k (Exp k a b) a) b 
        curry :: k (Prod k a b) c -> k a (Exp k b c)
        uncurry :: k a (Exp k b c) -> k (Prod k a b) c

instance CCC (->) where
	type Exp (->) = (->)
	type Prod (->) = (,)
	apply ~(f,x) = f x 
	curry f x y = f (x,y)
	uncurry f ~(x,y) = f x y 

{-
{-# RULES
"curry apply" curry apply = id
"curry . uncurry" curry . uncurry = id
"uncurry . curry" uncurry . curry = id
 #-}
-}

{-
class CoCCC k where
        data Coexp k :: * -> * -> *
	data Sum k :: * -> * -> *
        coapply :: k b (Sum k (Coexp k a b) a)
        cocurry :: k c (Sum k a b) -> k (Coexp k b c) a
        uncocurry :: k (Coexp k b c) a -> k c (Sum k a b)

{-# RULES
"cocurry coapply"       	cocurry coapply = id
"cocurry . uncocurry"        	cocurry . uncocurry = id
"uncocurry . cocurry"   	uncocurry . cocurry = id
 #-}
-}
