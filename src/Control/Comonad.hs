{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Comonad
-- Copyright   :  (C) 2008 Edward Kmett
--		  (C) 2004 Dave Menendez
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module declares the 'Comonad' class
----------------------------------------------------------------------------
module Control.Comonad where

import Control.Monad
import Control.Arrow ((|||), (&&&), (+++), (***))

infixl 1 =>>, .>>

{-|
There are two ways to define a comonad:

I. Provide definitions for 'fmap', 'extract', and 'duplicate'
satisfying these laws:

> extract . duplicate      == id
> fmap extract . duplicate == id
> duplicate . duplicate    == fmap duplicate . duplicate

II. Provide definitions for 'extract' and 'extend'
satisfying these laws:

> extend extract      == id
> extract . extend f  == f
> extend f . extend g == extend (f . extend g)

('fmap' cannot be defaulted, but a comonad which defines
'extend' may simply set 'fmap' equal to 'liftW'.)

A comonad providing definitions for 'extend' /and/ 'duplicate',
must also satisfy these laws:

> extend f  == fmap f . duplicate
> duplicate == extend id
> fmap f    == extend (f . duplicate)

(The first two are the defaults for 'extend' and 'duplicate',
and the third is the definition of 'liftW'.)
-}

class Functor w => Comonad w where
        duplicate :: w a -> w (w a)
        extend :: (w a -> b) -> w a -> w b
        extract :: w a -> a
        extend f = fmap f . duplicate
        duplicate = extend id

liftW :: Comonad w => (a -> b) -> w a -> w b
liftW f = extend (f . extract)

-- | 'extend' with the arguments swapped. Dual to '>>=' for monads.
(=>>) :: Comonad w => w a -> (w a -> b) -> w b
(=>>) = flip extend

-- | Injects a value into the comonad.
(.>>) :: Comonad w => w a -> b -> w b
w .>> b = extend (\_ -> b) w

-- | Transform a function into a comonadic action
liftCtx :: Comonad w => (a -> b) -> w a -> b
liftCtx f = extract . fmap f

mapW :: Comonad w => (w a -> b) -> w [a] -> [b]
mapW f w | null (extract w) = []
         | otherwise        = f (fmap head w) : mapW f (fmap tail w)

parallelW :: Comonad w => w [a] -> [w a]
parallelW w | null (extract w) = []
            | otherwise        = fmap head w : parallelW (fmap tail w)

unfoldW :: Comonad w => (w b -> (a,b)) -> w b -> [a]
unfoldW f w = fst (f w) : unfoldW f (w =>> snd . f)

-- | Converts a list of comonadic functions into a single function
-- returning a list of values
sequenceW :: Comonad w => [w a -> b] -> w a -> [b]
sequenceW []     _ = []
sequenceW (f:fs) w = f w : sequenceW fs w

