module Test.Lens.GetterSpec where

import Prelude

import Data.Profunctor.Strong (class Strong)
import Lens.Getter (view)
import Lens.Internal.Forget (Forget)
import Lens.Lens (lens)
import Lens.Types (AGetter, Fold, Lens', Optic, Lens)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Undefined (undefined)

type BoxRec = { value :: String }
data Box = Box BoxRec

instance showBox :: Show Box where
  show (Box b) = _.value b
instance eqBox :: Eq Box where
  eq (Box b1) (Box b2) = _.value b1 == _.value b2

explanation :: Unit
explanation = let
    {-
      lens (\(Box a) -> a) (\_ -> Box)
      は `Lens'` や `AGetter` として扱うことができる
      具体的な中身を見たいので一つずつ展開できなくなるまで展開してみる
    -}
    l = lens (\(Box a) -> a) (\_ -> Box)

    -- Lens'の展開
    _ = l :: Lens' Box BoxRec
    _ = l :: Lens Box Box BoxRec BoxRec
    _ = l :: forall p. Strong p => Optic p Box Box BoxRec BoxRec
    _ = l :: forall p. Strong p => p BoxRec BoxRec -> p Box Box

    -- AGetterの展開
    _ = l :: AGetter Box Box BoxRec BoxRec
    _ = l :: Fold BoxRec Box Box BoxRec BoxRec
    _ = l :: Optic (Forget BoxRec) Box Box BoxRec BoxRec
    _ = l :: (Forget BoxRec BoxRec BoxRec) -> (Forget BoxRec Box Box)
  in unit

_Box :: Lens' Box BoxRec
_Box = lens (\(Box a) -> a) (\_ -> Box)

_BoxExpanded :: forall p. Strong p => p BoxRec BoxRec -> p Box Box
_BoxExpanded = lens (\(Box a) -> a) (\_ -> Box)

forget2String :: ((Forget BoxRec BoxRec BoxRec) -> (Forget BoxRec Box Box)) -> String
forget2String _ = ""

-- 完全に展開した関数同士で型が合うか確かめるだけのもの
example :: String
example = forget2String _BoxExpanded

-- x :: forall r. Forget r String Int -> Unit
-- x _ = unit 

-- y :: forall p. Strong p => p String Int
-- y = (\_ -> "10")

-- -- Forget r a b の r が何だろうと、Strong p => p a b と a b の型が合っていれば渡せる
-- z :: Unit
-- z = x y

spec :: Spec Unit
spec = do
  describe "Getter Test" do
    it "lens" do
      let
        {-

          view :: forall s t a b. AGetter s t a b -> s -> a
          の`AGetter`の定義をわかりやすく展開してみる

          [3つの定義]
          type AGetter s t a b = Fold a s t a b
          type Fold r s t a b = Optic (Forget r) s t a b
          type Optic p s t a b = p a b -> p s t

          [展開したもの]
          type AGetter s t a b = Forget a a b -> Forget s s t

          viewに`Lens'`を渡しているので、`Lens'`も展開してみる
          type Lens' = forall p. Strong p => Optic p s s a a
          type Lens' = forall p. Strong p => p a b -> p s t

          `Forget`は`Strong`のinstanceになっているので、`view`に渡せる
          つまり`_Box`の型アノテーションは`AGetter Box Box BoxRec BoxRec`でもコンパイルエラーにならない

        -}
        box = view _Box (Box {value: "Value"})
      box `shouldEqual` {value: "Value"}

