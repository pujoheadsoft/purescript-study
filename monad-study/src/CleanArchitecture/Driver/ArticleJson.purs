module CleanArchitecture.Driver.ArticleJson where

import Prelude

type ArticleIndexJson = {id :: String}

type ArticleJson = {title :: String, body :: String, author :: String}
