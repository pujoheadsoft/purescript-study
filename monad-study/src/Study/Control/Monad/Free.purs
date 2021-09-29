module Study.Control.Monad.Free where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.Trans.Class (class MonadTrans)
import Data.CatList (CatList, empty, snoc, uncons)
import Data.Either (Either(..))
import Data.Eq (class Eq1, eq1)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Maybe (Maybe(..))
import Data.Ord (class Ord1, compare1)
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..))
import Unsafe.Coerce (unsafeCoerce)

{-
  CatListについて
  CatList は Catenable List の実装
  Catenabe Listはすべての操作をO(1)時間で実現するらしい
  (https://www.slideshare.net/yuganda/pfds-1021-lists-with-efficient-catenation)
  consはリストの先頭に要素を追加、snocはリストの最後に要素を追加する
  unconsは最初の要素と残りの要素からなるTupleに分解する
-}

{-
  Freeは展開するとこうなる
  Free 
    (FreeView f Val Val) 
    (CatList (ExpF (Val -> Free f Val)))
  FreeView と CatList を持っているが、CatListはFreeを持っており再帰的な定義になっている。
-}
data Free f a = Free (FreeView f Val Val) (CatList (ExpF f))

newtype ExpF f = ExpF (Val -> Free f Val)

data FreeView f a b = 
    Return a 
  | Bind (f b) (b -> Free f a)

-- 型計算に必要な型（データは不要なので型だけでよい）
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

instance freeMonadTrans :: MonadTrans Free where
  lift = liftF

instance freeMonadRec :: MonadRec (Free f) where
  tailRecM k a = k a >>= case _ of
    Loop b -> tailRecM k b
    Done r -> pure r

instance foldableFree :: (Functor f, Foldable f) => Foldable (Free f) where
  foldMap f = go
    where
    go = resume >>> case _ of
      Left fa -> foldMap go fa
      Right a -> f a
    
  foldl f = go
    where
    go r = resume >>> case _ of
      Left fa -> foldl go r fa
      Right a -> f r a
  
  foldr f = go
    where
    go r = resume >>> case _ of
      Left fa -> foldr (flip go) r fa
      Right a -> f a r

instance traversableFree :: Traversable f => Traversable (Free f) where
  traverse f = go
    where
    go = resume >>> case _ of
      Left fa -> join <<< liftF <$> traverse go fa
      Right a -> pure <$> f a
  sequence tma = traverse identity tma

{-
  任意のFunctor fをFreeに包まれたFunctor fに変換する
  (~>は自然変換。あるFunctorを別のFunctorに変換する)
-}
liftF :: forall f. f ~> Free f
liftF f = fromView (Bind (unsafeCoerceF f) (pure <<< unsafeCoerceVal))
  -- BindはFreeViewの型コンストラクタで、Bindの定義は、Bind (f b) (b -> Free f a)
  where
  unsafeCoerceF :: forall a. f a -> f Val -- Functor a を Functor Valに変換
  unsafeCoerceF = unsafeCoerce

  unsafeCoerceVal :: forall a. Val -> a -- 型Valを任意の値に変換
  unsafeCoerceVal = unsafeCoerce

{-
  レイヤーを追加
-}
wrap :: forall f a. f (Free f a) -> Free f a
wrap f = fromView (Bind (unsafeCoerceF f) unsafeCoerceVal)
  where
  unsafeCoerceF :: forall b. f (Free f b) -> f Val
  unsafeCoerceF = unsafeCoerce

  unsafeCoerceVal :: forall b. Val -> Free f b
  unsafeCoerceVal = unsafeCoerce

{-
  applicative functor `f` を与えられた値を free monad にサスペンドします。
  (型は変わらない)
-}
suspendF :: forall f. Applicative f => Free f ~> Free f
suspendF f = wrap (pure f)

{-
  自然変換を使って、フリーモナドの生成型コンストラクタを変更します。
-}
hoistFree :: forall f g. (f ~> g) -> Free f ~> Free g
hoistFree k = substFree (liftF <<< k)

{-
  型コンストラクタ `f` から末尾再帰的なモナド `m` への自然な変換で、自由なモナドを実行します。
  詳細は `MonadRec` 型クラスを参照してください。
-}
foldFree :: forall f m. MonadRec m => (f ~> m) -> Free f ~> m
foldFree k = tailRecM go
  where
  go :: forall a. Free f a -> m (Step (Free f a) a)
  go f = case toView f of
    Return a -> Done <$> pure a
    Bind g i -> (Loop <<< i) <$> k g

{-
  foldFree`のようなものですが、`MonadRec`のようなオーバーヘッドなしに、他のFreeモナドにフォールディングするためのものです。
-}
substFree :: forall f g. (f ~> Free g) -> Free f ~> Free g
substFree k = go
  where
  go :: Free f ~> Free g
  go f = case toView f of
    Return a -> pure a
    Bind g i -> k g >>= go <$> i

{-
  Functor `f` のレイヤーを一度にアンラップする関数を持つフリーモナドを実行します。
-}
runFree :: forall f a. Functor f => (f (Free f a) -> Free f a) -> Free f a -> a
runFree k free = go free
  where
  go :: Free f a -> a
  go f = case toView f of         -- FreeをFreeViewに変換
    Return a -> a                 -- Returnなら内容をそのまま返す
    Bind g i -> go (k (i <$> g))  -- BindならFreeを返す関数`k`を呼び、再帰。
                                  -- 以下はここの詳細 -------------------
                                  -- Bindの定義は Bind (f b) (b -> Free f a) なので gは(f b)、iは(b -> Free f a)
                                  -- Functorのfmap(<$>)で g の内容 b に 関数 i を適用。結果は (f (Free f a))。(fmapはgのfで包まれたまま返すから)
                                  -- この結果を関数 k に食わす。それをgoに渡して再帰している。
                                  -- Returnになるまで再帰され、最終的に a が返る

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

-- FreeViewからFreeに変換
fromView :: forall f a. FreeView f a Val -> Free f a
fromView f = Free (unsafeCoerceFreeView f) empty
  where
  unsafeCoerceFreeView :: FreeView f a Val -> FreeView f Val Val
  unsafeCoerceFreeView = unsafeCoerce

-- FreeからFreeViewに変換
toView :: forall f a. Free f a -> FreeView f a Val
toView (Free v s) = -- vはFreeView、sはCatList
  case v of
    Return a ->
      case uncons s of
        Nothing ->
          Return (unsafeCoerceVal a)
        Just (Tuple h t) ->
          toView (unsafeCoerceFree (concatF ((runExpF h) a) t))
    Bind f k ->
      Bind f (\a -> unsafeCoerceFree (concatF (k a) s))
      -- Bind (f b) (b -> Free f a)
  where
  -- 内部で持つCatList l と r を結合して返す(<>はCatListの<>。CatListはSemigroupの実装でもある)
  concatF :: Free f Val -> CatList (ExpF f) -> Free f Val
  concatF (Free v' l) r = Free v' (l <> r)

  -- ExpFの中身を取り出す
  runExpF :: ExpF f -> (Val -> Free f Val)
  runExpF (ExpF k) = k

  unsafeCoerceFree :: Free f Val -> Free f a
  unsafeCoerceFree = unsafeCoerce

  unsafeCoerceVal :: Val -> a
  unsafeCoerceVal = unsafeCoerce

