module Pattern.ThreeLayer.Layer1 where

import Prelude

import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Pattern.ThreeLayer.Layer2 (class GetUserName, class LogToScreen)
import Pattern.ThreeLayer.Layer3 (Name(..))

{-
  Layer 1
-}

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


instance LogToScreen AppM where
  log = liftEffect <<< log

instance GetUserName AppM where
  getUserName = do
    env <- ask
    liftEffect do
      -- 文字列を生成する何らかのeffect
      pure $ Name $ "some name " <> show env.someValue
