module Pattern.ThreeLayer.Layer2 where

import Prelude

import Pattern.ThreeLayer.Layer3 (Name, getName)

-- Capability type classes:
class Monad m <= LogToScreen m where
  log :: String -> m Unit
 
class Monad m <= GetUserName m where
  getUserName :: m Name
 
-- capabilitiesを利用するビジネスロジック
-- これによりテストが容易になる
program
  :: forall m
   . LogToScreen m 
  => GetUserName m
  => m Unit
program = do
  log "What is your name?"
  name <- getUserName
  log $ "You name is " <> getName name