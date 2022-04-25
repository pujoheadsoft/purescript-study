module Study.Control.Monad.Run where

import Prelude

--import Control.Monad.Free (Free, liftF, resume', runFree)
import Study.Control.Monad.Free (Free, resume', liftF, runFree)
import Data.Functor.Variant (VariantF, inj)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol)
import Data.Either (Either(..))
import Prim.Row as Row
import Partial.Unsafe (unsafeCrashWith)

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


resume
  :: forall a b r
   . (VariantF r (Run r a) -> b)
  -> (a -> b)
  -> Run r a
  -> b
resume k1 k2 = resume' (\x f -> k1 (Run <<< f <$> x)) k2 <<< unwrap

-- VariantFを受け取って、Runを返す
-- liftFでFreeモナドを作ってRunに食わせている
send :: forall a r. VariantF r a -> Run r a
send v = (Run <<< liftF) v

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

extract :: forall a. Run () a -> a
extract = unwrap >>> runFree \_ -> unsafeCrashWith "Run: the impossible happend"