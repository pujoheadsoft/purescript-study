module Main where

import Prelude

import CleanArchitecture.Driver.ArticleApiDriver as ArticleApiDriver
import CleanArchitecture.Main as CA
import CleanArchitecture.TaglessFinal as TaglessFinal
import CleanArchitecture.Usecase.FindArticleUsecase as FindArticleUsecase
import CleanArchitecture.Usecase.FindArticleUsecase2 as FindArticleUsecase2
import Effect (Effect)
import Example.Free.FreeExample as FreeExample
import Example.Free.SimpleFreeExample as SimpleFreeExample
import Example.Run.Example as RunExample
import Example.Run.Example2 as RunExample2
import Example.Run.ExampleOnPursuit as ExampleOnPursuit
import Study.Control.Monad.Free.FreeReader as FR
import Study.Control.Monad.Run.Reader (READER, ask)
import Study.Control.Monad.Run.Run (Run)
import Study.Control.Monad.Simple.Reader as R
import Type.Row (type (+))

readWithPlus :: R.Reader String String
readWithPlus = do
  value <- R.ask
  pure (value <> " Added!")

readWithPlusFree :: FR.FreeReader String String
readWithPlusFree = do
  value <- FR.ask
  pure (value <> " Added2!")

-- これはRunを返している
readWithPlusRun :: forall r. Run (READER Int + r) Int
readWithPlusRun = do
--  traceM({m: "readWithPlus10: ask (before)"})
  x <- ask
  y <- pure (x + 10)
--  traceM({m: "readWithPlus10: ask (after)", x: x})
  pure (y + 20) -- RunはApplicativeを実装しているのでpureはRun。

main :: Effect Unit
main = do
  -- T.main
  -- log $ R.runReader readWithPlus "hoge1"
  -- log $ FR.runReader readWithPlusFree "hoge1"
  -- RunExample.main
  RunExample.main
  RunExample2.main
  -- ExampleOnGithub.main >>= logShow
  -- trace(extract (runReader readWithPlusRun 100)) \_ -> log("")
  FreeExample.main
  SimpleFreeExample.main
  CA.main
  ExampleOnPursuit.main
  ArticleApiDriver.main
  FindArticleUsecase.main
  FindArticleUsecase2.main
  TaglessFinal.main
  