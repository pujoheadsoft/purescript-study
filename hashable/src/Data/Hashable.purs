module Data.Hashable where

import Prelude

import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))

newtype HashCode = HashCode Int

hashCode :: Int -> HashCode
hashCode h = HashCode (h `mod` 65535)

class Eq a <= Hashable a where
    hash :: a -> HashCode

instance showHashCode :: Show HashCode where
    show (HashCode h) = "(HashCode " <> show h <> ")"

derive instance eqHashCode :: Eq HashCode

instance hashBoolean :: Hashable Boolean where
    hash false = hashCode 0
    hash true = hashCode 1

instance hashInt :: Hashable Int where
    hash i = hashCode i

instance hashChar :: Hashable Char where
    hash c = (toCharCode >>> hash) c

instance hashString :: Hashable String where
    hash s = (toCharArray >>> hash) s

instance hashArray :: Hashable a => Hashable (Array a) where
    hash arr = foldl combineHashes (hashCode 0) (map hash arr)

instance hashMaybe :: Hashable a => Hashable (Maybe a) where
    hash Nothing = hashCode 0
    hash (Just a) = hashCode 1 `combineHashes` hash a

instance hashTuple :: (Hashable a, Hashable b) => Hashable (Tuple a b) where
    hash (Tuple a b) = hash a `combineHashes` hash b

instance hashEither :: (Hashable a, Hashable b) => Hashable (Either a b) where
    hash (Left a) = hashCode 0 `combineHashes` hash a
    hash (Right b) = hashCode 1 `combineHashes` hash b

combineHashes :: HashCode -> HashCode -> HashCode
combineHashes (HashCode h1) (HashCode h2) = hashCode (73 * h1 + 51 * h2)

hashEqual :: forall a. Hashable a => a -> a -> Boolean
hashEqual = eq `on` hash
-- 上記はこういうことをやっている。onの定義を見ればわかる: hashEqual a b = hash a `eq` hash b