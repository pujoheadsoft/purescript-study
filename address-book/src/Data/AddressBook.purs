module Data.AddressBook where

import Data.Maybe
import Prelude
import Control.Plus (empty)
import Data.List (List(..), filter, head)

type Entry
  = { firstName :: String
    , lastName :: String
    , address :: Address
    }

type Address
  = { street :: String
    , city :: String
    , state :: String
    }

type AddressBook
  = List Entry

showEntry :: Entry -> String
showEntry entry =
  entry.lastName <> ", "
    <> entry.firstName
    <> ": "
    <> showAddress entry.address

showAddress :: Address -> String
showAddress addr =
  addr.street <> ", "
    <> addr.city
    <> ", "
    <> addr.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry entry book = Cons entry book
-- eta conversion: insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName book  = (filter filterEntry >>> head) book
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName
