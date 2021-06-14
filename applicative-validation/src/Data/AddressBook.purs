module Data.AddressBook where

import Prelude

newtype Address
  = Address
  { street :: String
  , city :: String
  , state :: String
  }

address :: String -> String -> String -> Address
address street city state = Address { street, city, state }

data PhoneType
  = HomePhone
  | WorkPhone
  | CellPhone
  | OtherPhone

newtype PhoneNumber
  = PhoneNumber
  { "type" :: PhoneType
  , number :: String
  }

phoneNumber :: PhoneType -> String -> PhoneNumber
phoneNumber ty number =
  PhoneNumber
    { "type": ty
    , number: number
    }

newtype Person
  = Person
  { firstName :: String
  , lastName :: String
  , homeAddress :: Address
  , phones :: Array PhoneNumber
  }

person :: String -> String -> Address -> Array PhoneNumber -> Person
person firstName lastName homeAddress phones = Person { firstName, lastName, homeAddress, phones }

instance showAddress :: Show Address where
  show (Address a) =
    "Address "
      <> "{ street:"
      <> show a.street
      <> ", city: "
      <> show a.city
      <> ", state: "
      <> show a.state
      <> "}"

instance showPhoneType :: Show PhoneType where
  show HomePhone = "HomePhone"
  show WorkPhone = "WorkPhone"
  show CellPhone = "CellPhone"
  show OtherPhone = "OtherPhone"

instance showPhoneNumber :: Show PhoneNumber where
  show (PhoneNumber p) =
    "PhoneNumber "
      <> "{ type: "
      <> show p."type"
      <> ", number: "
      <> show p.number
      <> " }"

instance showPerson :: Show Person where
  show (Person p) =
    "Person "
      <> "{ firstName: "
      <> show p.firstName
      <> ", lastName: "
      <> show p.lastName
      <> ", homeAddress: "
      <> show p.homeAddress
      <> ", phones: "
      <> show p.phones
      <> " }"

examplePerson :: Person
examplePerson =
  person "John" "Smith"
    (address "123 Fake St." "FakeTown" "CA")
    [ phoneNumber HomePhone "555-555-5555"
    , phoneNumber CellPhone "555-555-0000"
    ]
