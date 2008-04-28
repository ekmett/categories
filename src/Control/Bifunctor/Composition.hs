module Control.Bifunctor.Composition where

newtype FunctorBT f p a b = FunctorBT { runFunctorBT :: f (p a b) } 
newtype SwapBT p a b = SwapB { runSwapBT :: p b a } 
newtype CompBT p f g a b = CompBT { runCompBT :: p (f a b) (g a b) }
newtype ArrowBT f g a b = ArrowBT { runArrowBT :: f a b -> g a b }
