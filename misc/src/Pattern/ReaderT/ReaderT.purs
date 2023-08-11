module Pattern.ReaderT.ReaderT where

import Prelude

import Control.Monad.Reader (class MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State as State
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref

{-
  https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/
-}

data Env = Env { 
  envLog :: String -> Effect Unit,
  envBalance :: Ref Int
}

class HasLog a where
  getLog :: a -> (String -> Effect Unit)
instance HasLog (String -> Effect Unit) where
  getLog = identity
else instance HasLog Env where
  getLog (Env {envLog}) = envLog

class HasBalance a where
  getBalance :: a -> Ref Int
instance HasBalance (Ref Int) where
  getBalance = identity
instance HasBalance Env where
  getBalance (Env {envBalance}) = envBalance

class Monad m <= MonadBalance m where
  modifyBalance :: (Int -> Int) -> m Unit
instance (HasBalance env, MonadEffect m) => MonadBalance (ReaderT env m) where
  modifyBalance f = do
    env <- ask
    liftEffect $ Ref.modify_ f (getBalance env)
instance Monad m => MonadBalance (State.StateT Int m) where
  modifyBalance = State.modify_

modify :: forall m. MonadBalance m => (Int -> Int) -> m Unit
modify f = do
  -- Now I know there's no way I'm performing IO here
  modifyBalance f

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
  runReaderT (logSomething "message") log
