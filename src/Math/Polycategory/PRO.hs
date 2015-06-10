{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Math.Polycategory.PRO where

import Math.Category
import Math.Rec
import Data.Type.Equality

class Category p => PRO p where
  pro :: p as bs -> p cs ds -> p (as ++ cs) (bs ++ ds)

instance (PRO p, Yoneda p ~ Op p) => PRO (Yoneda p) where
  pro (Op p) (Op q) = Op (pro p q)

instance PRO (:~:) where
  pro Refl Refl = Refl
