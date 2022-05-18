module Study.Control.Monad.Run
  ( Run(..)
  , extract
  , lift
  , peel
  , resume
  , run
  , send
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Functor.Variant (VariantF, inj)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol)
import Debug (debugger, spy, spyWith, trace, traceM)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Study.Control.Monad.Free (Free, resume', liftF, runFree)

-- data VariantF :: Row (Type -> Type) -> Type -> Type
-- data VariantF f a
newtype Run r a = Run (Free (VariantF r) a)

derive instance newtypeRun :: Newtype (Run r a) _ -- Newtypeの派生にしている
derive newtype instance functorRun :: Functor (Run r)
derive newtype instance applyRun :: Apply (Run r)
derive newtype instance applicativeRun :: Applicative (Run r)
derive newtype instance bindRun :: Bind (Run r)
derive newtype instance monadRun :: Monad (Run r)


run
  :: forall m a r
   . Monad m
  => (VariantF r (Run r a) -> m (Run r a))
  -> Run r a
  -> m a
run k = loop
  where
  loop :: Run r a -> m a
  loop = resume (\a -> loop =<< k a) pure

{-
  proxyとfunctorを受け取って、Runを返す
  functorをRunに持ち上げる(lift)？という意味合いか
-}
lift
  :: forall proxy symbol tail row f a
  . Row.Cons symbol f tail row
  => IsSymbol symbol
  => Functor f
  => proxy symbol -- proxy ここまでの定義で、↓のRunのrowはこのsymbolを持っていないといけない
  -> f a          -- functor 型aは↓のRunの型aと一致
  -> Run row a    -- Run
lift p f = trace({m: "Run: lift", proxy: p, f: f}) \_ -> (Run <<< liftF <<< inj p) f
{-
  [liftの説明]
  inj p は タグを指定してバリアントに値を取り込む関数。即ち VariantF(symbolの名前 :: 型) となる。値はf。
  (Variantのinjは、Functorを受け取れるようになっており、その場合の型はVariantFになる)

  liftFは、Functor fをFreeに包まれたFunctor fに変換する。即ち Free VariantF(symbolの名前 :: 型)

  更に↑のFreeをRunで包む。即ち Run (Free VariantF(symbolの名前 :: 型))

  Runの定義が、Run (Free (VariantF r) a)なのできっちり当てはまる
-}

{-
  Runを受け取ってEitherを返す
  Runの中のFreeの中のFreeViewの型がBindだったらLeftが返り、ReturnだったらRightが返る。

  この関数とresume関数の内容は、Free.pursのresume関数に酷似している。

  [Run.pursのresumeの内容をpeelが渡している引数で展開したもの]
  resume' (\x f -> Left (Run <<< f <$> x)) Right <<< unwrap

  [Freeのresume]
  resume = resume' (\g i -> Left (i <$> g)) Right
-}
peel
  :: forall a r
  . Run r a
  -> Either (VariantF r (Run r a)) a

peel r = trace({m: "Run: peel"}) \_ -> resume Left Right r -- Runも渡している

{-
[peelの説明]

peelは Either (VariantF r (Run r a)) a を戻り値の型としている
つまりresumeの定義のbがこれになる。
これとLeft Rightと省略されているRunをresumeの定義にあてはめるとこうなる
(LeftもRightも何かを受け取ってbを返す関数、(x -> b)という形の関数のため、peelに渡せる)

resume
  :: forall a b r
   . (VariantF r (Run r a) -> b) -- Left  つまり (VariantF r (Run r a) -> Either (VariantF r (Run r a)) a)
  -> (a -> b)                    -- Right つまり (a                    -> Either (VariantF r (Run r a)) a)
  -> Run r a                     -- Run r a
  -> b                           -- Either (VariantF r (Run r a)) a
resume k1 k2 run = resume'
  (\x f -> k1 (Run <<< f <$> x))
  k2 <<< unwrap
  run

resume' 
  :: forall f a r -- Freeの定義からfはFreeViewで、aはCatListなはず。
  . (forall b. f b -> (b -> Free f a) -> r) -- FreeViewである f b と、b を Freeに変換する関数を受け取って、rに変換する関数
  -> (a -> r) -- CatList a から r を返す関数
  -> Free f a
  -> r
-}

resume
  :: forall a b r
   . (VariantF r (Run r a) -> b)
  -> (a -> b)
  -> Run r a
  -> b
resume k1 k2 = trace({m: "Run: resume"}) \_ -> resume' (\x f -> k1 (Run <<< f <$> x)) k2 <<< unwrap
-- resume k1 k2 z = ((resume' (\x f -> k1 (Run <<< f <$> x)) k2) <<< unwrap) z この2つと同じ意味
-- resume k1 k2 z = resume' (\x f -> k1 (Run <<< f <$> x)) k2 (unwrap z)
{-
  この関数は b を返すようになっている。
  そして引数で受け取っている2つの関数がどちらも b を返す。
　更に1つ目の引数の (RUn r a) と3つ目の引数は一致している。

内容の説明(↓のresume'の定義を参照しつつ)
  (\x f -> k1 (Run <<< f <$> x)) : 定義をもとに展開すると (\(f b) (b -> Free f a) -> k1 (Run <<< f <$> x)) となる。
                                   つまり (f b) の b に (b -> Free f a) をfmapし (f (Free f a)) としたものを Run に食わせて更にk1を適用する。
                                   ↑のpeelだとk1はLeft

  k2 これは k2 をそのまま渡している
  unwrap これはRunの中身を取り出している。すなわち (Free (VariantF r) a)。

[resume'の定義] (関数が返す型が、1つ目と2つ目の関数が返す型と一致しているのは↑と同じ)
resume' 
  :: forall f a r -- Freeの定義からfはFreeViewで、aはCatListなはず。
  . (forall b. f b -> (b -> Free f a) -> r) -- FreeViewである f b と、b を Freeに変換する関数を受け取って、rに変換する関数
  -> (a -> r) -- CatList a から r を返す関数
  -> Free f a
  -> r

[Freeの定義]
data Free f a = Free (FreeView f Val Val) (CatList (ExpF f))
-}

-- VariantFを受け取って、Runを返す
-- liftFでFreeモナドを作ってRunに食わせている
send :: forall a r. VariantF r a -> Run r a
send v = (Run <<< liftF) v

extract :: forall a. Run () a -> a
extract r = trace("Run: extract") \_ -> 
  (unwrap >>> runFree \_ -> unsafeCrashWith "Run: the impossible happend") r
