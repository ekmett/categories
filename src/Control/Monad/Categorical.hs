module Control.Monad.Categorical where
import Prelude hiding (id,(.))
import Control.Category
import Control.Functor.Categorical

class CFunctor m c d => CPointed m c d where
	creturn :: d a (m a)

-- class CPointed m (~>) => CApplicative m (~>) where
--	cap :: m (a ~> b) ~> m a ~> m b

class CPointed m (~>) (~>) => CMonad m (~>) where
	cbind :: (a ~> m b) -> m a ~> m b
	cjoin :: m (m a) ~> m a 

	cbind f = cjoin . cmap f 
	cjoin = cbind id 
