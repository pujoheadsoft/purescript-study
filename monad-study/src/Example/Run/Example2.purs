module Example.Run.Example2 where

import Prelude

import Control.Monad.Rec.Class (Step(..))
import Data.Functor.Variant (on)
import Study.Control.Monad.Run.Run (Run(..), lift, runPure)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

{-
  DIできるようにしたい
  gatewayが使うdriverをDIするなど
-}

type User = {}

class Monad m <= UserGateway m where
  findById :: String -> m User

-- find user ---------------------------------------------
data FindUser a = FindUserById String (User -> a)

derive instance functorFindUser :: Functor FindUser

type FIND_USER r = (findUser :: FindUser | r)

_findUser = Proxy :: Proxy "findUser"

findUserById :: forall r. String -> Run (FIND_USER + r) User
findUserById id = lift _findUser (FindUserById id identity) 

handleFindUser :: forall a. FindUser a -> a
handleFindUser (FindUserById id reply) = reply {}

runFindUser :: 
  forall r a
   . (FindUser (Run (FIND_USER + r) a) -> Run (FIND_USER + r) a)
  -> Run (FIND_USER + r) a
  -> Run r a
runFindUser f r = runPure (\v -> on _findUser (Loop <<< f) Done v) r

-- find user option ----------------------------------------