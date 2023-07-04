module Test.Lens.GetterSpec where

import Prelude

import Data.Newtype (unwrap)
import Data.Profunctor.Strong (class Strong)
import Data.Tuple (Tuple(..))
import Lens.Getter (view)
import Lens.Internal.Forget (Forget(..))
import Lens.Lens (lens, lens')
import Lens.Record (prop)
import Lens.Types (AGetter, AGetter', Fold, Lens, Lens', Optic, Getter')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Prelude (Proxy(..))
import Type.Proxy (Proxy)

type BoxRec = { value :: String }
data Box = Box BoxRec

instance showBox :: Show Box where
  show (Box b) = _.value b
instance eqBox :: Eq Box where
  eq (Box b1) (Box b2) = _.value b1 == _.value b2

newtype Container = Container Box
derive newtype instance showContainer :: Show Container
derive newtype instance eqContainer :: Eq Container

_Box :: Lens' Box BoxRec
_Box = lens (\(Box a) -> a) (\_ -> Box)

_Container :: Lens' Container Box
_Container = lens (\(Container b) -> b) (\_ -> Container)

foo :: forall a b r. Lens { foo :: a | r } { foo :: b | r } a b
foo = prop (Proxy :: Proxy "foo")

fooGetter :: forall x. Getter' { foo :: x } x
fooGetter = foo


_Box' :: forall p. Strong p => p BoxRec BoxRec -> p Box Box
_Box' = _Box

_Container' :: forall p. Strong p => p Box Box -> p Container Container
_Container' = _Container

-- 合成
composed1 :: forall p. Strong p => p BoxRec BoxRec -> p Container Container
composed1 = _Container <<< _Box

-- 合成をLensで書いた
composed2 :: Lens' Container BoxRec
composed2 = _Container <<< _Box

-- 合成をForgetで書いた (Forget (BoxRec -> BoxRec) -> Forget (Container -> BoxRec))
composed3 :: Forget BoxRec BoxRec BoxRec -> Forget BoxRec Container Container
composed3 = _Container <<< _Box


spec :: Spec Unit
spec = do
  describe "Getter Test" do
    it "view" do
      let
        box = view _Box (Box {value: "Value"})
      box `shouldEqual` {value: "Value"}
    
    it "view nested" do
      let
        {-
          view (_Container <<< _Box) としたとき選択されるinsntaceは`Forget`なので
          _Box は Forget BoxRec BoxRec BoxRec -> Forget BoxRec Box Box
          _Container は Forget Box Box Box -> Forget Box Container Container
          となる。
          Forget BoxRec BoxRec BoxRec -> Forget BoxRec Container Container
        -}
        box = view (_Container <<< _Box) (Container $ Box {value: "Value"})
      box `shouldEqual` {value: "Value"}
    
    it "prop" do
      let
        x = view fooGetter { foo: 100 }
      x `shouldEqual` 100

{-
  `Lens`と`AGetter`は同じものとして扱うことができることを説明する関数
  具体的な中身を見たいので一つずつ展開できなくなるまで展開している
-}
explanation :: Unit
explanation = let
    l = lens (\(Box a) -> a) (\_ -> Box)

    -- Lens'の展開
    _ = l :: Lens' Box BoxRec
    _ = l :: Lens Box Box BoxRec BoxRec
    _ = l :: forall p. Strong p => Optic p Box Box BoxRec BoxRec
    _ = l :: forall p. Strong p => p BoxRec BoxRec -> p Box Box

    -- AGetterの展開
    _ = l :: AGetter' Box BoxRec
    _ = l :: AGetter Box Box BoxRec BoxRec
    _ = l :: Fold BoxRec Box Box BoxRec BoxRec
    _ = l :: Optic (Forget BoxRec) Box Box BoxRec BoxRec
    _ = l :: (Forget BoxRec BoxRec BoxRec) -> (Forget BoxRec Box Box)
  in unit

{-
  完全に展開した`Lens`を、完全に展開した`AGetter`に渡してみる説明の関数
  `Strong p => p BoxRec BoxRec -> p Box Box`
  を
  `(Forget BoxRec BoxRec BoxRec) -> (Forget BoxRec Box Box)`
  に渡すことができている(展開しただけだから当たり前なのだが)

  よりシンプルに考えると`Strong p => p BoxRec BoxRec`を`Forget BoxRec BoxRec BoxRec`に渡せるということになるが、それはなぜか？
  こう渡したときこの`p`は、何が選択されるのだろう？

  コンパイルされたjsを見ると、このようになっており、`p`は`Forget`が選択されている。
  var _BoxExpanded1 = _BoxExpanded(Data_Lens_Internal_Forget.strongForget);

  `getter2Unit`とか`viewExpanded`とかは`Forget`を受け取るようになっており、`Forget r`は`Strong`のインスタンスになっているから選択できるというわけか。
  `Strong p`のように抽象的な型を返しているがゆえに、そのインスタンスになっている型を受ける様々な関数に渡せるわけだ。
-}
explanationZ :: Unit
explanationZ = let
  _ = getter2Unit _BoxExpanded -- unit
  _ = viewExpanded _BoxExpanded (Box {value: "Value"}) -- { value: "Value" }
  in unit
  where
  -- AGetterを展開した型を受ける関数
  getter2Unit :: ((Forget BoxRec BoxRec BoxRec) -> (Forget BoxRec Box Box)) -> Unit
  getter2Unit _ = unit

  -- Getterの`view`関数を型を具体的にし且つ完全に展開した関数
  viewExpanded :: ((Forget BoxRec BoxRec BoxRec) -> (Forget BoxRec Box Box)) -> Box -> BoxRec
  viewExpanded l box = (unwrap (l (Forget identity))) box

  -- Lens型ではなく展開した型を返す
  _BoxExpanded :: forall p. Strong p => p BoxRec BoxRec -> p Box Box
  _BoxExpanded = lensExpanded (\(Box a) -> a) (\_ -> Box)

  -- Lensを完全に展開した型を返す
  lensExpanded :: forall s t a b p. (s -> a) -> (s -> b -> t) -> Strong p => p a b -> p s t
  lensExpanded get set = lens' (\s -> (Tuple (get s) \b -> set s b))


{-
  `Forget r String Int`型が、`Strong p => p String Int`として扱え、
  さらに（条件によっては）`Strong p => p String Int`型が、`Forget r String Int`型として扱えるということを説明する関数。

  [`Forget`型の値を`id`関数に渡せる理由]
  `Forget r`は`instance strongForget :: Strong (Forget r) where`のように`Strong`型のインスタンスなので、
  `Strong`型クラスの制約を満たす（`Strong p => p String Int`の`p`として正しい）。
  だから渡せる。

  [`id`関数で返した`Strong p => p String Int`型の値が、`foget2Unit`関数に渡せる理由]
  `id`関数は`Strong p => p String Int`を受けてそのまま返しているが、`Forget`型を渡した場合、自動的に`Forget`型が返るよう解釈されるため、
  `id`関数で返した値を`forget2Unit`関数に渡せている。
-}
explanationX :: Unit
explanationX = forget2Unit $ id (Forget (\_ -> 100)) -- Intを返しているが`r`は任意の型なので返すのは何でもいい。
  where                                              -- `r`を適当な型にした場合、同じ型の値を返さないとコンパイルエラーになる
  -- Forget型をとる関数
  forget2Unit :: forall r. Forget r String Int -> Unit
  forget2Unit _ = unit

  -- `Strong`型クラスの制約を満たす任意の型`p`をとり、同じ型を返すだけの関数
  id :: forall p. Strong p => p String Int -> p String Int
  id = identity

{-
  `Strong p => p String Int`型に、単なる関数も`Forget`も両方渡せることを説明する関数。
-}
explanationY :: Unit
explanationY = let
  _ = strong2Unit (\_ -> 100) -- 関数`->`は`Strong p`の制約を満たす`p`になりえるので、動作する
  in strong2Unit forget
  where
  -- Strong p String Int型をとる関数
  strong2Unit :: forall p. Strong p => p String Int -> Unit
  strong2Unit _ = unit

  -- forget型の値を返す。この場合は、`Forget r`の`r`は明示しないといけない（この場合は`Int`）。
  forget :: Forget Int String Int
  forget = Forget (\_ -> 100)