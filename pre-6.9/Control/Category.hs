module Control.Category 
	( (>>>), (<<<), Category ((.), id)
	) where

import Prelude hiding (id,(.))
import qualified Prelude hiding (flip)

class Category (~>) where
	(.) :: (b ~> c) -> (a ~> b) -> a ~> c
	id :: a ~> a

(<<<) :: Category (~>) => (b ~> c) -> (a ~> b) -> a ~> c
(<<<) = (.)

(>>>) :: Category (~>) => (a ~> b) -> (b ~> c) -> a ~> c
(>>>) = flip (.)

instance Category (->) where
	(.) = (Prelude..)
	id = Prelude.id
