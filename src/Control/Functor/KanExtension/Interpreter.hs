{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Functor.KanExtension.Interpreter
-- Copyright   :  (C) 2008 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (rank-2 polymorphism)
--
-- Ghani and Johann's Interp/InterpT types from ''Initial Algebra Semantics is Enough!''
-- <http://crab.rutgers.edu/~pjohann/tlca07-rev.pdf> and its dual.
----------------------------------------------------------------------------
module Control.Functor.KanExtension.Interpreter
	( Interpreter, InterpreterT
	, interpreterAlgebra, algebraInterpreter
	, Cointerpreter, CointerpreterT
	, cointerpreterCoalgebra, coalgebraCointerpreter
	) where

import Control.Functor.Extras
import Control.Functor.HigherOrder
import Control.Functor.KanExtension

type Interpreter y g h = y :~> Ran g h
type InterpreterT f g h = forall y. Functor y => Interpreter y g h -> Interpreter (f y) g h

interpreterAlgebra :: InterpreterT f g h -> HAlgebra f (Ran g h)
interpreterAlgebra i = i id

algebraInterpreter :: HFunctor f => HAlgebra f (Ran g h) -> InterpreterT f g h
algebraInterpreter h i = h . hfmap i

type Cointerpreter y g h = Lan g h :~> y
type CointerpreterT f g h = forall y. Functor y => Cointerpreter y g h -> Cointerpreter (f y) g h

cointerpreterCoalgebra :: CointerpreterT f g h -> HCoalgebra f (Lan g h)
cointerpreterCoalgebra i = i id

coalgebraCointerpreter :: HFunctor f => HCoalgebra f (Lan g h) -> CointerpreterT f g h
coalgebraCointerpreter h i = hfmap i . h
