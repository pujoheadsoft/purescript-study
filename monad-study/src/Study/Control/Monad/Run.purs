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

-- proxyとfunctorを受け取って、Runを返す
lift
  :: forall proxy symbol tail row f a
  . Row.Cons symbol f tail row
  => IsSymbol symbol
  => Functor f
  => proxy symbol
  -> f a
  -> Run row a
lift p f = (Run <<< liftF <<< inj p) f

{-
  Runを受け取ってEitherを返す
-}
peel
  :: forall a r
  . Run r a
  -> Either (VariantF r (Run r a)) a

peel = resume Left Right -- Runも渡している

{-
peelの説明

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
-- resume k1 k2 z = ((resume' (\x f -> k1 (Run <<< f <$> x)) k2) <<< unwrap) z この2つと同じ意味
-- resume k1 k2 z = resume' (\x f -> k1 (Run <<< f <$> x)) k2 (unwrap z)

-- VariantFを受け取って、Runを返す
-- liftFでFreeモナドを作ってRunに食わせている
send :: forall a r. VariantF r a -> Run r a
send v = (Run <<< liftF) v

extract :: forall a. Run () a -> a
extract = unwrap >>> runFree \_ -> unsafeCrashWith "Run: the impossible happend"
