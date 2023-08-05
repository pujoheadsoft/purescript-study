module Pattern.ReaderT where

import Prelude

import Control.Monad.Reader (class MonadReader, ReaderT, ask, runReaderT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log, logShow)
import Undefined (undefined)

{-
  https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/
-}

data Env = Env { envLog :: String -> Effect Unit }

class HasLog a where
  getLog :: a -> (String -> Effect Unit)
instance HasLog (String -> Effect Unit) where
  getLog = identity
else instance HasLog Env where
  getLog (Env {envLog}) = envLog


{-
  `ReaderT`は`MonadReader`や`MonadEffect`のインスタンスになっている。

  `MonadReader`は`MonadAsk`が上位クラスになっている。
  class MonadAsk r m <= MonadReader r m | m -> r where

  そして`ReaderT`は`MonadAsk`のインスタンスになっている
  instance monadAskReaderT :: Monad m => MonadAsk r (ReaderT r m) where
    ask = ReaderT pure

  `MonadReader`のインスタンスにもなっている
  instance monadReaderReaderT :: Monad m => MonadReader r (ReaderT r m) where
    local = withReaderT

  これらの`ask`や`local`は`ReaderT`を返す

  `ReaderT`は`MonadEffect`のインスタンスにもなっている
  class Monad m <= MonadEffect m where
    liftEffect :: forall a. Effect a -> m a

  `liftEffect`して`lift`している。`lift`は以下の通り、`ReaderT`で包まれる。
  instance monadEffectReader :: MonadEffect m => MonadEffect (ReaderT r m) where
    liftEffect = lift <<< liftEffect

  class MonadTrans t where
    lift :: forall m a. Monad m => m a -> t m a

  instance monadTransReaderT :: MonadTrans (ReaderT r) where
    lift = ReaderT <<< const

  runReaderTは
  runReaderT :: forall r m a. ReaderT r m a -> (r -> m a)
-}
logSomething
  :: forall m env
   . HasLog env
  => MonadReader env m 
  => MonadEffect m
  => String
  -> m Unit
logSomething msg = do
  env <- ask
  liftEffect $ getLog env msg

main :: Effect Unit
main = do
  let
    env = Env { envLog: log }
  runReaderT (logSomething "message") env
