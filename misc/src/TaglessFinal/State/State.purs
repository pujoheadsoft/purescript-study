module TaglessFinal.State.State where

import Prelude

import TaglessFinal.Domain.Article (Article)

type State
  = { article :: Article }