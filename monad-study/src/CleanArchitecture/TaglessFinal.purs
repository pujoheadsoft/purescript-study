module CleanArchitecture.TaglessFinal where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String.Regex (search)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Study.Control.Monad.Run.Except (EXCEPT, catch)
import Study.Control.Monad.Run.Reader (READER, ask, runReader)
import Study.Control.Monad.Run.Run (EFFECT, Run, AFF, runBaseEffect, send)
import Type.Row (type (+))

type Key = String
type Value = String

type Price = Int

class Monad m <= DataStoreSYM m where
  create :: Key -> Value -> m Unit
  read   :: Key          -> m (Maybe Value)
  update :: Key -> Value -> m Unit

class Monad m <= BitCoinSYM m where
  getPrice :: m Price

newtype X e a = X {hoge :: String | a}

newtype EffectWithReader e r a = EffectWithReader (Run (EFFECT + READER e + r) a)
derive newtype instance functorX :: Functor (EffectWithReader e r)
derive newtype instance applyX :: Apply (EffectWithReader e r)
derive newtype instance applicativeX :: Applicative (EffectWithReader e r)
derive newtype instance bindX :: Bind (EffectWithReader e r)
derive newtype instance monadX :: Monad (EffectWithReader e r)

instance DataStoreSYM (EffectWithReader String r) where
  create k v = EffectWithReader do 
    env <- ask
    logShow $ "create " <> k <> v <> env
  read   k   = EffectWithReader do
    env <- ask
    pure $ Just $ "read by " <> k <> env
  update k v = EffectWithReader do
    env <- ask
    logShow $ "update " <> k <> v <> env

instance BitCoinSYM (EffectWithReader Int r) where
  getPrice = EffectWithReader do
    v <- ask
    logShow "getPrice"
    pure (v + 100)

createOrUpdate :: forall m. DataStoreSYM m => Key -> Value -> m Unit
createOrUpdate k v = do
  peek <- read k
  case peek of
    Just _  -> update k v
    Nothing -> create k v

saveBTCPrice :: forall m. BitCoinSYM m => DataStoreSYM m => m Unit
saveBTCPrice = do
  price <- getPrice
  createOrUpdate "BTC Price: " $ show price

run :: forall e r a. EffectWithReader e r a → Run ( EFFECT + READER e + r ) a
run (EffectWithReader r) = r


main :: Effect Unit
main = do
  log "----- Tagless Final Start ------------------"
  run (createOrUpdate "Key" "Value")
    # flip runReader ""
    # runBaseEffect

  run getPrice
    # flip runReader 100
    # runBaseEffect
    >>= logShow -- 戻り値の型はEffect Intなので、そのままlogShowには突っ込めない value <- で受けて出すか、こうやってbindさせてやって表示する
  
  -- run saveBTCPrice
  --   # flip runReader ""
  --   # flip runReader 1000
  --   # runBaseEffect
  log "----- Tagless Final End --------------------"
  