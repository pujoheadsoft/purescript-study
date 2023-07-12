module Study.Control.Monad.Free.Free where

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
import Debug (debugger, spy, spyWith, trace, traceM)
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
  FreeView と CatList を持っているが、CatListはExpFの中にFreeを持っており再帰的な定義になっている。

  Freeの種は Free :: (Type → Type) → Type → Type なので
  f は種が (Type -> Type) となるものでないといけない。わかりやすい例で言えばMaybeとか。
  この制約は、FreeViewの定義の(f b)の部分によって発生している
-}
data Free f a = Free (FreeView f Val Val) (CatList (ExpF f))

{-
  Val から Free f Val を返す関数
-}
newtype ExpF f = ExpF (Val -> Free f Val)

{-
  ReturnとBindは、Applyのpure関数とBindのbind関数を型で表現している。
  Return: 単にaを持っているだけ。
  Bind: (f b) と bを受け取ってFree f aを返す関数
  bind関数の定義が、forall a b. m a -> (a -> m b) -> m b であることを考えると構造が似ていることがわかる。
  命名はよくわからんが、こいつは型で分岐するための重要な要素っぽい。
-}
data FreeView f a b = 
    Return a 
  | Bind (f b) (b -> Free f a)

-- | 型計算に必要な型（データは不要なので型だけでよい）
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
  map :: forall a b. (a -> b) -> Free f a -> Free f b
  map k f = pure <<< k =<< f

instance freeBind :: Bind (Free f) where
  bind :: forall a b. Free f a -> (a -> Free f b) -> Free f b
  bind (Free v s) k = 
    -- 普通のbindは関数だが、FreeのbindはFree自体を返す
    -- このFreeは、引数のview(Bind or Return)と同じviewを持ち、CatListにbindの関数kを結合した新しいFreeとなる
    -- ちなみに一連の処理で最初にくるFreeは↓pureで作られたFree(jsをdebugするとわかる)
    Free v (snoc s (ExpF (unsafeCoerceBind k)))
    where
    unsafeCoerceBind :: (a -> Free f b) -> Val -> Free f Val
    unsafeCoerceBind = unsafeCoerce

instance freeApplicative :: Applicative (Free f) where
  pure :: forall a. a -> Free f a
  pure = fromView <<< Return

instance freeApply :: Apply (Free f) where
  apply :: forall a b. Free f (a -> b) -> Free f a -> Free f b
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
  Freeの持つCatListは空になっている
-}
liftF :: forall f. f ~> Free f
liftF f 
  -- BindはFreeViewの型コンストラクタ。Bindの定義: Bind (f b) (b -> Free f a)
  -- unsafeCoerceFは単なる変換
  -- (pure <<< unsafeCoerceVal)は合成関数。
  -- pureはfromView <<< Returnで、ReturnはFreeViewなので
  -- fromViewの定義: forall f a. FreeView f a Val -> Free f a から Bindの(b -> Free f a)に合致する
  -- つまり(pure <<< unsafeCoerceVal)はほとんど何の手も加えない
  = fromView (Bind (unsafeCoerceF f) (pure <<< unsafeCoerceVal))
  where
  unsafeCoerceF :: forall a. f a -> f Val -- Functor a を Functor Valに変換
  unsafeCoerceF a = unsafeCoerce a

  unsafeCoerceVal :: forall a. Val -> a -- 型Valを任意の値に変換
  unsafeCoerceVal a = unsafeCoerce a

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

-- |
-- | 型コンストラクタ `f` から末尾再帰的なモナド `m` への自然な変換で、自由なモナドを実行します。
-- | 詳細は `MonadRec` 型クラスを参照してください。
-- |
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

runFreeM
  :: forall f m a
   . Functor f
  => MonadRec m
  => (f (Free f a) -> m (Free f a))
  -> Free f a
  -> m a
runFreeM k = tailRecM go
  where
  go :: Free f a -> m (Step (Free f a) a)
  go f = case toView f of
    Bind g i -> Loop <$> k (i <$> g)
    Return a -> Done <$> pure a

{- 
  Functor f のレイヤーを一つだけunwrapし、Eitherに包んで返す。
-}
resume
  :: forall f a
  . Functor f => Free f a
  -> Either (f (Free f a)) a
resume = resume' (\g i -> Left (i <$> g)) Right

{- 
  Functor f のレイヤーを一つだけunwrapし、その続きの処理を提供する。
-}
resume' 
  :: forall f a r -- Freeの定義からfはFreeViewで、aはCatListなはず。
  . (forall b. f b -> (b -> Free f a) -> r) -- FreeViewである f b と、b を Freeに変換する関数を受け取って、rに変換する関数
  -> (a -> r) -- CatList a から r を返す関数
  -> Free f a
  -> r
resume' k j f = case toView f of -- Free f を toViewに渡して FreeView に変換
  Bind g i ->
    k g i -- Bindだったらkを適用
  Return a ->
    j a -- Returnだったらjを適用

{-
  FreeViewからFreeに変換
-}
fromView :: forall f a. FreeView f a Val -> Free f a
fromView f = Free (unsafeCoerceFreeView f) empty -- FreeViewと空のCatListでFreeを作る
  where
  unsafeCoerceFreeView :: FreeView f a Val -> FreeView f Val Val
  unsafeCoerceFreeView = unsafeCoerce

{-
  FreeからFreeViewに変換する
  FreeとFreeViewが持っている f a は同じ型
  f a は任意の型とはしているが、Freeの定義的にfはFreeViewだし、aはCatListとなる。
  参考: Free (FreeView f Val Val) (CatList (ExpF f))
        FreeView f a b = Return a | Bind (f b) (b -> Free f a)
        ExpF f = ExpF (Val -> Free f Val)
-}
toView :: forall f a. Free f a -> FreeView f a Val
toView (Free v s) =
  case v of
    -- FreeViewがReturnだった場合
    Return a ->
      case uncons s of -- uncons は Catenableリストを最初の要素と残りの要素からなる `Tuple` に分解する。
        -- CatListがない場合は、単にaを型変換してReturnに包んで返す。ReturnはFreeView。aはFree f Valになっているということか。
        Nothing ->
          Return (unsafeCoerceVal a)
        -- ある場合はheadとtailのTupleに分解される。tailはCatList。CatListの型はExpF f。
        Just (Tuple h t) ->
          -- 概要: CatListのheadが示す関数に値aを渡した結果を
          -- 1. runExpF h を実行して (Val -> Free h Val)
          -- 2. (Val -> Free h Val) a を実行して Free h a
          -- 3. ConcatF (Free h a) t を実行して (Free h a) と t を結合したCatListを持つFreeすなわち(Free h (a <> t))にする
          -- 4. unsafeCoerceFreeして型変換しつつ再帰呼び出し
          --    これをunsafeで呼び出してOKということはライブラリ的に、hは絶対FreeViewになっているということか。
          toView (unsafeCoerceFree (concatF ((runExpF h) a) t)) -- runExpFで{m: 'readWithPlus10: ask (before)'}
    -- FreeViewがBindだったら新たなBindを返す
    Bind f k ->
      -- もとの入れ物fはそのままで、関数だけ変換する
      -- 返るのはBindなので型的にはFreeViewとなり正しい。
      --   新たな関数の説明:
      --   渡された値xに元の関数kを適用させる。kは(b -> Free f a)という定義なので、Free f aが返る。
      --   そのFree f aと大元のFree v sが持っていたCatList sを結合したFree f (a <> s)とする。
      Bind f (\x -> unsafeCoerceFree (concatF (k x) s))
  where
  -- 内部で持つCatList l と r を結合して返す(<>はCatListの<>。CatListはSemigroupの実装でもある)
  -- FreeViewはそのままで、CatListだけが結合された新たなFreeが返る
  concatF :: Free f Val -> CatList (ExpF f) -> Free f Val
  concatF (Free v' l) r = Free v' (l <> r)

  -- ExpFの中身(Val -> Free f Val)を取り出す
  runExpF :: ExpF f -> (Val -> Free f Val)
  runExpF (ExpF k) = k

  -- Free の中身を変換して返す
  unsafeCoerceFree :: Free f Val -> Free f a
  unsafeCoerceFree = unsafeCoerce

  -- Val から a に変換して返す
  unsafeCoerceVal :: Val -> a
  unsafeCoerceVal = unsafeCoerce

