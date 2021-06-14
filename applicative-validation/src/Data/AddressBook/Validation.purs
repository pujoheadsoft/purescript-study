module Data.AddressBook.Validation where

import Prelude

import Data.AddressBook (Address(..), Person(..), PhoneNumber(..), address, person, phoneNumber)
import Data.Either (Either(..))
import Data.String (length)
import Data.String.Regex (Regex, regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, invalid, unV)
import Partial.Unsafe (unsafePartial)

type Errors
  = Array String

nonEmpty :: String -> String -> V Errors Unit
nonEmpty field "" = invalid [ "Field '" <> field <> "' cannot be empty" ]

nonEmpty _ _ = pure unit

arrayNonEmpty :: forall a. String -> Array a -> V Errors Unit
arrayNonEmpty field [] = invalid [ "Field '" <> field <> "' must contain at least one value" ]

arrayNonEmpty _ _ = pure unit

lengthIs :: String -> Int -> String -> V Errors Unit
lengthIs field len value
  | (length value) /= len = invalid [ "Field '" <> field <> "' must have length " <> show len ]

-- | はガード valueの長さがlenと一致しないという条件が満たされた場合このケースにマッチする
lengthIs _ _ _ = pure unit

phoneNumberRegex :: Regex
phoneNumberRegex = unsafePartial case regex "^\\d{3}-\\d{3}-\\d{4}$" noFlags of Right r -> r

matches :: String -> Regex -> String -> V Errors Unit
matches _ regex value
  | test regex value = pure unit

matches field _ _ = invalid [ "Field '" <> field <> "' did not match the required format" ]

validateAddress :: Address -> V Errors Address
validateAddress (Address a) =
  address <$> (nonEmpty "Street" a.street *> pure a.street)
    <*> (nonEmpty "City" a.city *> pure a.city)
    <*> (lengthIs "State" 2 a.state *> pure a.state)

-- *> は Control.Applyに定義されている applySecond モナドをふたつとって合成し右オペランドの方を返す。
validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber (PhoneNumber p) =
  phoneNumber <$> pure p."type"
    <*> (matches "Number" phoneNumberRegex p.number *> pure p.number)

validatePerson :: Person -> V Errors Person
validatePerson (Person p) =
    person <$> (nonEmpty "First Name" p.firstName *> pure p.firstName)
    <*> (nonEmpty "lastName Name" p.lastName *> pure p.lastName)
    <*> validateAddress p.homeAddress
    <*> (arrayNonEmpty "Phone Numbers" p.phones *> traverse validatePhoneNumber p.phones)

validatePerson' :: Person -> Either Errors Person
validatePerson' p = unV Left Right $ validatePerson p