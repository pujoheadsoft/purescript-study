module Pattern.FourLayer.Domain where

import Prelude

import Pattern.FourLayer.Core (Name, getName)

-- Layer 3 (Domain)
-- Three Layer Cake では Layer 2 に相当する
 
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