module CleanArchitecture.TaglessFinal where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Study.Control.Monad.Run.Reader (READER, ask)
import Study.Control.Monad.Run.Run (EFFECT, Run, runBaseEffect, send)
import Type.Row (type (+))

type Key = String
type Value = String

type Price = Int

class Monad m <= DataStoreSYM m where
  create :: Key -> Value -> m Unit
  read   :: Key -> Value -> m (Maybe Value)
  update :: Key -> Value -> m Unit

class Monad m <= BitCoinSYM m where
  getPrice :: m Price

instance DataStoreSYM (Run (EFFECT + READER String + ())) where
  create k v = do 
    env <- ask
    logShow "create " <> k <> v <> env
  read   k   = do
    env <- ask
    pure "read by " <> k 
  update k v = do
    env <- ask
    logShow "update " k <> v <> env

instance BitCoinSYM (Run (EFFECT + READER Int + r)) where
  getPrice = do
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

main :: Effect Unit
main = do
  log "Usecase Start ------------------"
  saveBTCPrice
    # runBaseEffect
  