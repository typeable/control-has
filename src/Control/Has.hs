
module Control.Has
  ( module X
  , Has(..)
  , HasAll
  , MonadHas
  , Fieldy
  , Generic
  ) where

import Control.Lens
import Control.Monad.Reader as X
import Data.Kind
import GHC.Generics as G


class Has x r where
  part :: Lens r r x x

instance {-# OVERLAPS #-} Has r r where
  part = id

type family HasAll (xs :: [Type]) r :: Constraint where
  HasAll '[] r = ()
  HasAll (x ': xs) r = (Has x r, HasAll xs r)

data LensList (r :: Type) (es :: [Type]) where
  LLNil :: LensList r '[]
  LLCons :: Lens r r e e -> LensList r es -> LensList r (e ': es)

mapLL :: Lens s s r r -> LensList r es -> LensList s es
mapLL _ LLNil         = LLNil
mapLL m (LLCons l ls) = LLCons (m . l) (mapLL m ls)

type family (xs :: [Type]) ++ (ys :: [Type]) :: [Type] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

concatLL :: LensList r es1 -> LensList r es2 -> LensList r (es1 ++ es2)
concatLL LLNil ys         = ys
concatLL (LLCons l ls) ys = LLCons l (concatLL ls ys)

-- borrowed from Data.HSet.Get
data N = Z | S N

type family IndexOf (x :: Type) (xs :: [Type]) :: N where
  IndexOf x (x ': _) = 'Z
  IndexOf x (_ ': xs) = 'S (IndexOf x xs)

class (i ~ IndexOf e es) => GetLens e es i | i es -> e where
  getLens :: LensList r es -> Lens r r e e

type Elem e es = GetLens e es (IndexOf e es)

class Fieldy (r :: Type) where
  type Fields r :: [Type]
  type Fields r = GFields (Rep r)
  lensList :: LensList r (Fields r)
  default lensList
    :: (Generic r, GFieldy (Rep r), Fields r ~ (GFields (Rep r)))
    => LensList r (Fields r)
  lensList = mapLL (iso G.from G.to) glensList

instance {-# OVERLAPPABLE #-} (Fieldy r, Elem e (Fields r)) => Has e r where
  part = getLens lensList

type MonadHas es r m = (MonadReader r m, HasAll es r)

instance GetLens e (e ': es) 'Z where
  getLens (LLCons l _) = l

instance (GetLens e' es n, 'S n ~ IndexOf e' (e ': es))
  => GetLens e' (e ': es) ('S n) where
  getLens (LLCons _ ls) = getLens ls

class GFieldy (r :: Type -> Type) where
  type GFields r :: [Type]
  glensList :: LensList (r x) (GFields r)

instance GFieldyProduct r => GFieldy (D1 d (C1 c r)) where
  type GFields (D1 d (C1 c r)) = GFieldsProduct r
  glensList = mapLL (iso unM1 M1 . iso unM1 M1) glensListProduct

class GFieldyProduct (r :: Type -> Type) where
  type GFieldsProduct r :: [Type]
  glensListProduct :: LensList (r x) (GFieldsProduct r)

instance (GFieldyProduct r, GFieldyProduct s) => GFieldyProduct (r :*: s) where
  type GFieldsProduct (r :*: s) = GFieldsProduct r ++ GFieldsProduct s
  glensListProduct =
    concatLL
      (mapLL (lens lget lset) glensListProduct)
      (mapLL (lens rget rset) glensListProduct)
    where
      lget (l :*: _) = l
      lset (_ :*: r) l = l :*: r
      rget (_ :*: r) = r
      rset (l :*: _) r = l :*: r

instance GFieldyProduct (S1 s (K1 k r)) where
  type GFieldsProduct (S1 s (K1 k r)) = '[r]
  glensListProduct = LLCons (iso unM1 M1 . iso unK1 K1) LLNil

type family AllElem (xs :: [Type]) (es :: [Type]) :: Constraint where
  AllElem '[] _ = ()
  AllElem (x ': xs) es = (Elem x es, AllElem xs es)

instance Fieldy (a, b)
instance Fieldy (a, b, c)
instance Fieldy (a, b, c, d)
instance Fieldy (a, b, c, d, e)
instance Fieldy (a, b, c, d, e, f)
instance Fieldy (a, b, c, d, e, f, g)
