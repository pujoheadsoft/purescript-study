module Pattern.FourLayer.APIWithInfrastructure where

import Prelude

import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Control.Monad.Reader.Trans (ask)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Pattern.FourLayer.Core (Name(..))
import Pattern.FourLayer.Domain (class GetUserName, class LogToScreen)

{-
  Layer 2 (API) と Layer 1 (Infrastructure) は同じmoduleに定義する
  (というかOrphanインスタンスになるのでこう定義せざるを得ない)
-}


-- Layer 2 (API: Production)
-- Three Layer Cake では Layer 1 に相当する

-- Environment type
type Environment = { someValue :: Int } -- mutable state, read-only values, etc. go in this record

-- newtyped ReaderT that implements the capabilities
newtype AppM a = AppM (ReaderT Environment Effect a)
derive newtype instance functorTestM    :: Functor AppM
derive newtype instance applyAppM       :: Apply AppM
derive newtype instance Applicative AppM
derive newtype instance bindAppM        :: Bind AppM
derive newtype instance monadAppM       :: Monad AppM
derive newtype instance monadEffect     :: MonadEffect AppM
derive newtype instance monadAsk        :: MonadAsk Environment AppM

runApp :: forall a. AppM a -> Environment -> Effect a
runApp (AppM r) env = runReaderT r env



-- Layer 1 (Infrastructure: 各インスタンスの実装)
-- Three Layer Cake ではこれも Layer 1 に相当する
instance LogToScreen AppM where
  log = liftEffect <<< log

instance GetUserName AppM where
  getUserName = do
    env <- ask
    liftEffect do
      -- 文字列を生成する何らかのeffect
      pure $ Name $ "some name " <> show env.someValue