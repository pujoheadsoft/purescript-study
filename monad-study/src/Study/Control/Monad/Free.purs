module Study.Control.Monad.Free where

import Prelude

import Data.CatList (CatList, empty, snoc, uncons)
import Data.Either (Either(..))
import Data.Eq (class Eq1, eq1)
import Data.Maybe (Maybe(..))
import Data.Ord (class Ord1, compare1)
import Data.Tuple (Tuple(..))
import Unsafe.Coerce (unsafeCoerce)


data Free f a = Free (FreeView f Val Val) (CatList (ExpF f))

newtype ExpF f = ExpF (Val -> Free f Val)

data FreeView f a b = Return a | Bind (f b) (b -> Free f a)

data Val


instance eqFree :: (Functor f, Eq1 f, Eq a) => Eq (Free f a) where
  eq x y = case resume x, resume y of
    Left fa, Left fb -> eq1 fa fb
    Right a, Right b -> a == b
    _, _ -> false

instance eq1Free :: (Functor f, Eq1 f) => Eq1 (Free f) where
  eq1 = eq

instance ordFree :: (Functor f, Ord1 f, Ord a) => Ord (Free f a) where
  compare x y = case resume x, resume y of
    Left fa, Left fb -> compare1 fa fb
    Left _, _ -> LT
    _, Left _ -> GT
    Right a, Right b -> compare a b

instance ord1Free :: (Functor f, Ord1 f) => Ord1 (Free f) where
  compare1 = compare

instance freeFunctor :: Functor (Free f) where
  map k f = pure <<< k =<< f

instance freeBind :: Bind (Free f) where
  bind (Free v s) k = Free v (snoc s (ExpF (unsafeCoerceBind k)))
    where
    unsafeCoerceBind :: forall a b. (a -> Free f b) -> Val -> Free f Val
    unsafeCoerceBind = unsafeCoerce

instance freeApplicative :: Applicative (Free f) where
  pure = fromView <<< Return

instance freeApply :: Apply (Free f) where
  apply = ap

instance freeMonad :: Monad (Free f)

resume :: forall f a. Functor f => Free f a -> Either (f (Free f a)) a
resume = resume' (\g i -> Left (i <$> g)) Right

resume' 
  :: forall f a r
  . (forall b. f b -> (b -> Free f a) -> r)
  -> (a -> r)
  -> Free f a
  -> r
resume' k j f = case toView f of
  Return a -> j a
  Bind g i -> k g i

toView :: forall f a. Free f a -> FreeView f a Val
toView (Free v s) =
  case v of
    Return a ->
      case uncons s of
        Nothing ->
          Return (unsafeCoerceVal a)
        Just (Tuple h t) ->
          toView (unsafeCoerceFree (concatF ((runExpF h) a) t))
    Bind f k ->
      Bind f (\a -> unsafeCoerceFree (concatF (k a) s))
  where
  concatF :: Free f Val -> CatList (ExpF f) -> Free f Val
  concatF (Free v' l) r = Free v' (l <> r)

  runExpF :: ExpF f -> (Val -> Free f Val)
  runExpF (ExpF k) = k

  unsafeCoerceFree :: Free f Val -> Free f a
  unsafeCoerceFree = unsafeCoerce

  unsafeCoerceVal :: Val -> a
  unsafeCoerceVal = unsafeCoerce

fromView :: forall f a. FreeView f a Val -> Free f a
fromView f = Free (unsafeCoerceFreeView f) empty
  where
  unsafeCoerceFreeView :: FreeView f a Val -> FreeView f Val Val
  unsafeCoerceFreeView = unsafeCoerce