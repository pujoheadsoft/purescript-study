module Example.Run.RunExample2 where

import Prelude

import Data.Either (Either(..))
import Data.Functor.Variant (match, on)
import Study.Control.Monad.Run.Run (Run, lift, peel, run, send)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

newtype User = User {id :: String, name :: String}

data UserPort a
  = FindUserById String a
  | FindUserByName String a

derive instance functorUserPort :: Functor UserPort

type USER_PORT r = (userPort :: UserPort | r)

_userPort = Proxy :: Proxy "userPort"

findUserById :: forall r. String -> Run (USER_PORT + r) Unit
findUserById id = lift _userPort (FindUserById id unit)

findUserByName :: forall r. String -> Run (USER_PORT + r) Unit
findUserByName name = lift _userPort (FindUserByName name unit)

handleUserPort :: forall a. UserPort a -> User
handleUserPort (FindUserById id _) = User {id: id, name: "dummy"}
handleUserPort (FindUserByName name _) = User {id: "dummy" , name: name}

-- runUserPort :: forall r a. Run (USER_PORT + r) a -> Run r User
-- runUserPort :: forall r a b. (UserPort a -> b) -> Run (USER_PORT + r ) b -> Run r b
-- runUserPort f r = case peel r of
--   Left a -> case on _userPort Left Right a of
--     Left a' -> pure (f a')
--     Right a' -> send a' >>= \r' -> runUserPort r'
--   Right a ->
--     pure a

-- main = do
--   runUserPort (findUserById "xxx")