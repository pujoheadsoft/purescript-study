module Test.Lens.SetterSpec where

import Prelude

import Lens.Lens (lens)
import Lens.Record (prop)
import Lens.Setter (set)
import Lens.Types (Lens, Lens', Setter')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

type BoxRec = { value :: String }
data Box = Box BoxRec

instance showBox :: Show Box where
  show (Box b) = _.value b
instance eqBox :: Eq Box where
  eq (Box b1) (Box b2) = _.value b1 == _.value b2

_Box :: Lens' Box BoxRec
_Box = lens (\(Box a) -> a) (\_ -> Box)

foo :: forall a b r. Lens { foo :: a | r } { foo :: b | r } a b
foo = prop (Proxy :: Proxy "foo")

fooSetter :: forall x. Setter' { foo :: x } x
fooSetter = foo

spec :: Spec Unit
spec = do
  describe "Setter Test" do
    it "set" do
      let
        {-
          setの実装では`{value: "NewValue"}`と`(Box {value: "Value"})`はそのまま`Lens`の構造に渡される。
        -}
        box = set _Box {value: "NewValue"} (Box {value: "Value"})
      box `shouldEqual` Box {value: "NewValue"}
    
    it "prop" do
      let
        x = set fooSetter 200 { foo: 100 }
      x `shouldEqual` { foo: 200 }