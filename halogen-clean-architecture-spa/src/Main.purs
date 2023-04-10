module Main where

import Prelude

import Component.Router as Router
import Effect (Effect)
import Effect.Console (log)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Test.Mock2 (mock, verify, (:), (:>))
import Unsafe.Coerce (unsafeCoerce)

-- main :: Effect Unit
-- main = HA.runHalogenAff do
--   body <- HA.awaitBody
--   runUI Router.component unit body

main :: Effect Unit
main = do
  let

    m = mock [(1 : "2") :> "r1",
              (2 : "3") :> "r2"]
    _ = m.fun 1 "2"
    _ = m.fun 2 "3"

    m2 = mock [(1 : "2") :> "r1",
              (2 : "3") :> "r2"]
    _ = m2.fun 1 "2"
    _ = m2.fun 2 "3"
    _ = m2.fun 1 "2"

  verify m (1 : "2")
  verify m (2 : "3")

--  log $ showVerify m2 (1 : "2")
--  verifyCount m (1 : "2") 2