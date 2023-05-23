module Test.Data.VariantSpec where

import Prelude

import Data.Variant (Variant, default, inj, onMatch)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

someFoo :: forall v. Variant (foo :: Int | v)
someFoo = inj (Proxy :: Proxy "foo") 42

someBar :: forall v. Variant (bar :: Boolean | v)
someBar = inj (Proxy :: Proxy "bar") true

someBaz :: forall v. Variant (baz :: String | v)
someBaz = inj (Proxy :: Proxy "baz") "Baz"

onFooOrBar :: forall v. (Variant v -> String) -> Variant (foo :: Int, bar :: Boolean | v) -> String
onFooOrBar = onMatch
  { foo: show :: Int -> String
  , bar: if _ then "true" else "false"
  }

spec :: Spec Unit
spec = do
  describe "Variantのテスト" do
    describe "onMatchのテスト" do
      it "matchする場合(Fooの場合)" do
        onFooOrBar (\_ -> "") someFoo `shouldEqual` "42"

      it "matchする場合(Barの場合)" do
        onFooOrBar (\_ -> "") someBar `shouldEqual` "true"

      it "matchしない場合は渡した関数の結果が使われる(bazzの場合)" do
        onFooOrBar (default "default") someBaz `shouldEqual` "default"

