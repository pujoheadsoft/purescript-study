module Example.Run.RunExample2 where

import Prelude

import Data.Either (Either(..))
import Data.Functor.Variant (on)
import Debug (traceM)
import Effect (Effect)
import Study.Control.Monad.Run.Run (Run, extract, lift, peel, send)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type User = {id :: String, name :: String}

data UserPort a
  = FindUserById String (User -> a)
  | FindUserByName String (User -> a)

derive instance functorUserPort :: Functor UserPort

type USER_PORT r = (userPort :: UserPort | r)

_userPort = Proxy :: Proxy "userPort"

findUserById :: forall r. String -> Run (USER_PORT + r) User
findUserById id = lift _userPort (FindUserById id identity)

findUserByName :: forall r. String -> Run (USER_PORT + r) User
findUserByName name = lift _userPort (FindUserByName name identity)

handleUserPort :: forall a. UserPort a -> a
handleUserPort (FindUserById id reply) = reply {id: id, name: "dummy"}
handleUserPort (FindUserByName name reply) = reply {id: "dummy" , name: name}

runUserPort :: forall r a. Run (USER_PORT + r) User -> Run a User
runUserPort r = case peel r of
  Left a -> case on _userPort Left Right a of
    Left a' -> runUserPort (handleUserPort a')
    Right a' -> send a' >>= \r' -> runUserPort r'
  Right a ->
    pure a

program :: forall r. String -> Run (USER_PORT + r) User
program id = do
  user <- findUserById id
  findUserByName user.id

main :: Effect Unit
main = do
  traceM(extract (runUserPort (program "userId")))
   