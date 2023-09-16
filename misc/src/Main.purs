module Main where

import Prelude

import Aff.Main as AffMain
import Aff.MyAff as MyAff
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay, forkAff, joinFiber, launchAff_, suspendAff)
import Effect.Class.Console (log)
import Pattern.FourLayer.Main as FourLayer
import Pattern.ReaderT.ReaderT as RederTPattern
import Pattern.ThreeLayer.Main as ThreeLayer


main :: Effect Unit
main = do
--   launchAff_ do
--     a <- forkAff do
-- --      delay $ Milliseconds 100.0
--       pure "a"

--     _ <- forkAff do
--       a' <- joinFiber a
--       log $ "bar" <> a'

--     _ <- forkAff do
--       a' <- joinFiber a
--       log $ "baz" <> a'

--     pure unit


  MyAff.launchAff_ do
    a <- MyAff.forkAff do
      MyAff.delay $ Milliseconds 100.0
      pure "a"

    _ <- MyAff.forkAff do
      a' <- MyAff.joinFiber a
      log $ "bar " <> a'

    _ <- MyAff.forkAff do
      a' <- MyAff.joinFiber a
      log $ "baz " <> a'

    pure unit

-- main :: Effect Unit
-- main = do
--   RederTPattern.main
--   ThreeLayer.main
--   FourLayer.main
--   AffMain.main
