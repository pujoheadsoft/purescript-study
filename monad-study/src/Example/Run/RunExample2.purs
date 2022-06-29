module Example.Run.RunExample2 where

import Prelude

import Data.Either (Either(..))
import Data.Functor.Variant (match, on)
import Debug (trace, traceM)
import Effect (Effect)
import Effect.Console (log)
import Study.Control.Monad.Run.Run (Run, extract, lift, peel, run, send)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

newtype User = User {id :: String, name :: String}

data UserPort :: forall k. k -> Type
data UserPort a
  = FindUserById String
  | FindUserByName String

derive instance functorUserPort :: Functor UserPort

type USER_PORT :: forall k. Row (k -> Type) -> Row (k -> Type)
type USER_PORT r = (userPort :: UserPort | r)

_userPort = Proxy :: Proxy "userPort"

findUserById :: forall a r. String -> Run (USER_PORT + r) a
findUserById id = lift _userPort (FindUserById id)

findUserByName :: forall a r. String -> Run (USER_PORT + r) a
findUserByName name = lift _userPort (FindUserByName name)

handleUserPort :: forall a. UserPort a -> User
handleUserPort (FindUserById id) = User {id: id, name: "dummy"}
handleUserPort (FindUserByName name) = User {id: "dummy" , name: name}

runUserPort :: forall r a. Run (USER_PORT + r) User -> Run a User
runUserPort r = case peel r of
  Left a -> case on _userPort Left Right a of
    Left a' -> pure (handleUserPort a')
    Right a' -> send a' >>= \r' -> runUserPort r'
  Right a ->
    pure a

main :: Effect Unit
main = do
   traceM(extract (runUserPort (findUserById "userId")))
   traceM(extract (runUserPort (findUserByName "userName")))