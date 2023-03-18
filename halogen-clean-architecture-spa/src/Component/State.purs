module Component.State where

import Prelude

import Domain.Article (Article)

type State
  = { article :: Article }