module Study.Control.Monad.Run.Reader
  ( READER
  , Reader(..)
  , _reader
  , ask
  , askAt
  , asks
  , asksAt
  , liftReader
  , liftReaderAt
  , runReader
  , runReaderAt
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Functor.Variant (on)
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Study.Control.Monad.Run.Run (Run)
import Study.Control.Monad.Run.Run as Run
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

newtype Reader e a = Reader (e -> a)

derive newtype instance functorReader :: Functor (Reader e)
-- instance functorReader :: Functor (Reader e) where
--   map f (Reader a) = debugger \_ -> Reader (\x -> f (a x))

type READER e r = (reader :: Reader e | r)

_reader :: Proxy "reader"
_reader = Proxy

liftReader :: forall e a r. Reader e a -> Run (READER e + r) a
liftReader = liftReaderAt _reader

liftReaderAt ::
  forall symbol tail row e a
   . IsSymbol symbol
  => Row.Cons symbol (Reader e) tail row
  => Proxy symbol
  -> Reader e a
  -> Run row a
liftReaderAt p r = Run.lift p r
-- proxyとReaderを渡す
-- Run.liftは proxy p とfunctor r を受け取るが、Readerは↑のderiveでFunctorになっているので渡すことができる
-- ここではpの値としてrが設定されたRunが返ってくる。こんな感じ Run (Free (VariantF (symbolの名前 :: 型))), symbolに紐づく値はr
-- Run (Free (VariantF r) a)

{-
  askは Run (READER e + r) e そのもの
  Readerの関数は identity なので、渡されたものをそのまま返すことになる。
  あとこれはaskAtを呼んだ結果がaskに代入されているみたいな感じ。つまりaskAtはaskが呼ばれたときに呼ばれるわけではない。
  (デバッグしてみると、askを呼ぶより前にAskAtが呼ばれていることからもわかる)
-}
ask :: forall e r. Run (READER e + r) e
ask = askAt _reader -- proxyを渡してRunを返す

{-
  呼び出しを追っていくと、これは↓のようになることがわかる。
    Run.lift sym (Reader identity)

  返ってくるのはこうなる
  Run (Free (VariantF (symの名前 :: 型))), symbolに紐づく値は(Reader identity)
-}
askAt ::
  forall t e r s
  . IsSymbol s
  => Row.Cons s (Reader e) t r
  => Proxy s
  -> Run r e
-- askAt sym = trace({m: "Reader: askAt", sym: sym}) \_ -> asksAt sym identity
askAt sym = asksAt sym identity

asks :: forall e r a. (e -> a) -> Run (READER e + r) a
asks = asksAt _reader

asksAt ::
  forall t e r s a
  . IsSymbol s
  => Row.Cons s (Reader e) t r
  => Proxy s
  -> (e -> a) -- この関数の引数の型は↑のReaderの型eと一致、更に返す型は↓のRunの型aと一致している
  -> Run r a -- 返すのはRun
asksAt sym f = liftReaderAt sym (Reader f) -- Readerは関数を持つのでfを渡せる

runReader ::
  forall e a r. 
  Run (READER e + r) a -- Run
  -> e
  -> Run r a -- 新たなRun
runReader r e = runReaderAt _reader r e -- e と Run が渡される

runReaderAt ::
  forall t e a r s
  . IsSymbol s -- Symbolである
  => Row.Cons s (Reader e) t r -- r は Symbol s の値として (Reader e) を持っていないといけない
  => Proxy s -- Symbol s
  -> Run r a -- Run r a
  -> e       -- 環境 e
  -> Run t a
runReaderAt sym run env = loop run env
  where
  -- symがあったらLeft、なかったらRightになる
  handle = on sym Left Right
  -- eは環境(例えばint値や文字列など)、rはRunの中身なのでFree
  loop r e = case Run.peel r of
    -- LeftってことはFreeViewがBindだったってこと
    -- このときの r は Free (Bind CatList) みたいな形
    Left a -> case handle a of
      -- symがあったら
      Left (Reader k) ->
        -- 環境eをkに渡した結果で再帰
        -- askを使っていた場合、この環境eが入ったFreeが返ってくる
        -- askを使ったときに値が取得できる理由がこれ。そしてBindで次のFreeに計算後の結果が渡っていく
        loop (k e) e
      -- symがなかったら(これはRunの中身が合成されてる場合？readr + wirterみたいな)
      Right a' ->
        Run.send a' >>= \r' -> runReaderAt sym r' e
    -- RightってことはFreeViewがReturnだったってこと
    -- このときの r は Free (Return CatList) みたいな形
    Right a ->
      pure a
