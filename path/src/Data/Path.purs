module Data.Path where

import Prelude
import Data.Maybe (Maybe(..))

data Path
  = Directory String (Array Path)
  | File String Int

instance showPath :: Show Path where
  show = filename

filename :: Path -> String
filename (File name _) = name

filename (Directory name _) = name

isDirectory :: Path -> Boolean
isDirectory (Directory _ _) = true

isDirectory _ = false

ls :: Path -> Array Path
ls (Directory _ xs) = xs

ls _ = []

size :: Path -> Maybe Int
size (File _ bytes) = Just bytes

size _ = Nothing

root :: Path
root =
  Directory "/"
    [ Directory "/bin/"
        [ File "/bin/ls" 1234
        , File "/bin/mv" 3456
        ]
    , Directory "/home/"
        [ Directory "/home/user/"
            [ File "/home/user/test.txt" 10 ]
        ]
    ]
