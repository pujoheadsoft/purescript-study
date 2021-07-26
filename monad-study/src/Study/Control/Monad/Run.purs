module Study.Control.Monad.Run where

import Prelude

import Control.Monad.Free (Free, liftF, resume', runFree)
import Data.Functor.Variant (VariantF, inj)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol)
import Data.Either (Either(..))
import Prim.Row as Row
import Type.Row (type (+))
import Effect (Effect)
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

-- liftFでFreeモナドを作ってRunに食わせている
send :: forall a r. VariantF r a -> Run r a
send v = (Run <<< liftF) v


lift
  :: forall proxy sym r1 r2 f a
  . Row.Cons sym f r1 r2
  => IsSymbol sym
  => Functor f
  => proxy sym
  -> f a
  -> Run r2 a
lift p = Run <<< liftF <<< inj p

peel
  :: forall a r
  . Run r a
  -> Either (VariantF r (Run r a)) a
peel = resume Left Right

extract :: forall a. Run () a -> a
extract = unwrap >>> runFree \_ -> unsafeCrashWith "Run: the impossible happend"