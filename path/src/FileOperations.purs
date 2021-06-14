module FileOperations where

import Prelude

import Data.Array (concatMap, (:))
import Data.Path (Path, ls)

allFiles :: Path -> Array Path
allFiles file = file : concatMap allFiles (ls file)

allFiles' :: Path -> Array Path
allFiles' file = file : do
    child <- ls file
    allFiles' child