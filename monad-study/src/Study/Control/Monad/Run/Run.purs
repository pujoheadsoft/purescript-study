module Study.Control.Monad.Run.Run where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Data.Either (Either(..))
import Data.Functor.Variant (VariantF, inj, match)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), curry, uncurry)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Study.Control.Monad.Free.Free (Free, resume', liftF, runFree, runFreeM)
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

-- data VariantF :: Row (Type -> Type) -> Type -> Type
-- data VariantF f a
newtype Run r a = Run (Free (VariantF r) a)

derive instance newtypeRun :: Newtype (Run r a) _ -- Newtypeの派生にしている
derive newtype instance functorRun :: Functor (Run r)
derive newtype instance applyRun :: Apply (Run r)
derive newtype instance applicativeRun :: Applicative (Run r)
derive newtype instance bindRun :: Bind (Run r)
derive newtype instance monadRun :: Monad (Run r)

instance monadRecRun :: MonadRec (Run r) where
  tailRecM f = loop
    where
    loop a = do
      b <- f a
      case b of
        Done r -> pure r
        Loop n -> loop n

{-
  proxyとfunctorを受け取って、Runを返す
  functorをRunに持ち上げる(lift)？という意味合いか
-}
lift
  :: forall symbol tail row f a
  . Row.Cons symbol f tail row
  => IsSymbol symbol
  => Functor f
  => Proxy symbol -- proxy ここまでの定義で、↓のRunのrowはこのsymbolを持っていないといけない
  -> f a          -- functor 型aは↓のRunの型aと一致
  -> Run row a    -- Run
lift p f = (Run <<< liftF <<< inj p) f
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
peel r = resume Left Right r -- Runも渡している

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
resume k1 k2 = resume' (\x f -> k1 (Run <<< f <$> x)) k2 <<< unwrap
{-
  この関数は b を返すようになっている。
  そして引数で受け取っている2つの関数がどちらも b を返す。
　更に1つ目の引数の (RUn r a) と3つ目の引数は一致している。
  Freeの方のresumeじゃなくてこっちでも定義しているのは、Runで包みたいからだと思われる

内容の説明(↓のresume'の定義を参照しつつ)
  (\x f -> k1 (Run <<< f <$> x)) : 定義をもとに展開すると (\(f b) (b -> Free f a) -> k1 (Run <<< f <$> x)) となる。
                                   ↑のpeelだとk1はLeft, Readerの例だとxはReader自体(Functor)

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
extract r = (unwrap >>> runFree \_ -> unsafeCrashWith "Run: the impossible happend") r

-- run系の関数 ------------------------------------------------------------------------
-- | プログラムの残りの部分を intercept できる
-- | interpret と同じだが、より制限の少ないシグニチャになっている
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

-- | モナド m を経由してプログラムから値を取り出す
interpret :: forall m a r. Monad m => (VariantF r ~> m) -> Run r a -> m a
interpret = run

runRec
  :: forall m a r
   . MonadRec m 
  => (VariantF r (Run r a) -> m (Run r a))
  -> Run r a
  -> m a
runRec k = runFreeM (coerceM k) <<< unwrap
  where
  coerceM 
    :: (VariantF r (Run r a) -> m (Run r a)) 
    -> VariantF r (Free (VariantF r) a) 
    -> m (Free (VariantF r) a)
  coerceM = unsafeCoerce
  
-- | m を経由したプログラムから、継続渡しを使って値を取り出す
runCont
  :: forall m a b r
   . (VariantF r (m b) -> m b)
  -> (a -> m b)
  -> Run r a
  -> m b
runCont k1 k2 = loop
  where
  loop :: Run r a -> m b
  loop = resume (\b -> k1 (loop <$> b)) k2

-- | Accumulatorを持つあるモナド `m` を介して、値を抽出する。
-- | これは再帰の下でのスタックセーフを仮定している。
runAccum
  :: forall m r s a
   . Monad m
  => (s -> VariantF r (Run r a) -> m (Tuple s (Run r a)))
  -> s
  -> Run r a
  -> m a
runAccum k = loop
  where
  loop :: s -> Run r a -> m a
  loop s = resume (\b -> uncurry loop =<< k s b) pure

-- | runAccumのMonadRec版
runAccumRec
  :: forall m r s a
   . MonadRec m
  => (s -> VariantF r (Run r a) -> m (Tuple s (Run r a)))
  -> s
  -> Run r a
  -> m a
runAccumRec k = curry (tailRecM (uncurry loop))
  where
  loop :: s -> Run r a -> m (Step (Tuple s (Run r a)) a)
  loop s = resume (\b -> Loop <$> k s b) (pure <<< Done)

-- | Accumulatorを用いた継続渡しにより、ある `m` を経由して値を抽出する。
runAccumCont
  :: forall m r s a b
   . (s -> VariantF r (s -> m b) -> m b)
  -> (s -> a -> m b)
  -> s
  -> Run r a
  -> m b
runAccumCont k1 k2 = loop
  where
  loop :: s -> Run r a -> m b
  loop s = resume (\b -> k1 s (flip loop <$> b)) (k2 s)

-- | 純粋にエフェクトを除去します。
-- | Control.Monad.Rec.Class` の `Step` を使用して、末尾再帰のスタックセーフを維持する。
runPure
  :: forall r1 r2 a
   . (VariantF r1 (Run r1 a) -> Step (Run r1 a) (VariantF r2 (Run r1 a)))
  -> Run r1 a
  -> Run r2 a
runPure k = loop
  where
  loop :: Run r1 a -> Run r2 a
  loop r = case peel r of
    Left a -> case k a of
      Loop r' -> loop r'
      Done a' -> send a' >>= runPure k
    Right a ->
      pure a

-- | Accumulatorで純粋にエフェクトを除去する。
-- | Control.Monad.Rec.Class` の `Step` を使用して、末尾再帰のスタックセーフを維持する。
runAccumPure
  :: forall r1 r2 a b s
   . (s -> VariantF r1 (Run r1 a) -> Step (Tuple s (Run r1 a)) (VariantF r2 (Run r1 a)))
  -> (s -> a -> b)
  -> s
  -> Run r1 a
  -> Run r2 b
runAccumPure k1 k2 = loop
  where
  loop :: s -> Run r1 a -> Run r2 b
  loop s r = case peel r of
    Left a -> case k1 s a of
      Loop (Tuple s' r') -> loop s' r'
      Done a' -> send a' >>= runAccumPure k1 k2 s
    Right a ->
      pure (k2 s a)

--------------------------------------------------------------------------

-- Effect と Aff ---------------------------------------------------------
type EFFECT r = (effect :: Effect | r)

liftEffect :: forall r. Effect ~> Run (EFFECT + r)
liftEffect = lift (Proxy :: Proxy "effect")

runBaseEffect :: Run (EFFECT + ()) ~> Effect
runBaseEffect = runRec $ match { effect: \a -> a }

type AFF r = (aff :: Aff | r)

liftAff :: forall r. Aff ~> Run (AFF + r)
liftAff = lift (Proxy :: Proxy "aff")

runBaseAff :: Run (AFF + ()) ~> Aff
runBaseAff = run $ match { aff: \a ->a }

instance runMonadEffect :: (TypeEquals (Proxy r1) (Proxy (EFFECT r2))) => MonadEffect (Run r1) where
  liftEffect = fromRows <<< liftEffect

instance runMonadAff :: (TypeEquals (Proxy r1) (Proxy (AFF + EFFECT + r2))) => MonadAff (Run r1) where
  liftAff = fromRows <<< liftAff

fromRows
  :: forall f r1 r2 a
   . TypeEquals (Proxy r1) (Proxy r2)
  => f r2 a
  -> f r1 a
fromRows = unsafeCoerce