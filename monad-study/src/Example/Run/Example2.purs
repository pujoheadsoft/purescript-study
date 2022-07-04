module Example.Run.Example2 where

import Prelude

import Control.Monad.Rec.Class (Step(..))
import Data.Functor.Variant (on)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Study.Control.Monad.Run.Run (Run, extract, lift, runPure)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

{-
  DIできるようにしたい
  gatewayが使うdriverをDIするなど
-}
type UserDomain = {user :: User, userOption :: UserOption}
--derive instance showUserDomain :: Show UserDomain

class UserGateway m where
  findById :: String -> m UserDomain

-- find user ---------------------------------------------
type User = {id :: String, name :: String}

data FindUser a = FindUserById String (User -> a)
derive instance functorFindUser :: Functor FindUser
type FIND_USER r = (findUser :: FindUser | r)
_findUser = Proxy :: Proxy "findUser"

findUserById :: forall r. String -> Run (FIND_USER + r) User
findUserById id = lift _findUser (FindUserById id identity) 

handleFindUser :: forall a. FindUser a -> a
handleFindUser (FindUserById id reply) = reply {id: id, name: "dummy"}

runFindUser :: 
  forall r a
   . (FindUser (Run (FIND_USER + r) a) -> Run (FIND_USER + r) a)
  -> Run (FIND_USER + r) a
  -> Run r a
runFindUser f r = runPure (\v -> on _findUser (Loop <<< f) Done v) r

-- find user option ----------------------------------------
type UserOption = {userId :: String, optionId :: String, label :: String}

data FindUserOption a = FindUserOptionById String (UserOption -> a)
derive instance functorFindUserOption :: Functor FindUserOption
type FIND_USER_OPTION r = (findUserOption :: FindUserOption | r)
_findUserOption = Proxy :: Proxy "findUserOption"

findUserOptionById :: forall r. String -> Run (FIND_USER_OPTION + r) UserOption
findUserOptionById id = lift _findUserOption (FindUserOptionById id identity) 

handleFindUserOption :: forall a. FindUserOption a -> a
handleFindUserOption (FindUserOptionById id reply) = reply {userId: id, optionId: "dummy", label: "dummy"}

runFindUserOption :: 
  forall r a
   . (FindUserOption (Run (FIND_USER_OPTION + r) a) -> Run (FIND_USER_OPTION + r) a)
  -> Run (FIND_USER_OPTION + r) a
  -> Run r a
runFindUserOption f r = runPure (\v -> on _findUserOption (Loop <<< f) Done v) r
-------------------------------------------------------

newtype X a = X (Run (FIND_USER + FIND_USER_OPTION + ()) a)
instance gatewayImpl :: UserGateway X where
  findById :: String -> X UserDomain
  findById id = X do 
    user <- findUserById id
    userOption <- findUserOptionById id
    pure {user: user, userOption: userOption}

runFindUserDomain :: String -> Run (FIND_USER + FIND_USER_OPTION + ()) UserDomain
runFindUserDomain id = case findById id of (X r) -> r

main :: Effect Unit
main = logShow <<< extract
  $ runFindUserDomain "userId"
  # runFindUser handleFindUser
  # runFindUserOption handleFindUserOption