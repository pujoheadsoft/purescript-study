module Study.Control.Monad.Run.Reader where

import Prelude
import Data.Symbol (class IsSymbol)
import Data.Either (Either(..))
import Type.Proxy (Proxy(..))
import Prim.Row as Row
import Type.Row (type (+))
import Data.Functor.Variant (on)
import Study.Control.Monad.Run (Run)
import Study.Control.Monad.Run as Run
import Debug (debugger, spy, spyWith, trace, traceM)

newtype Reader e a = Reader (e -> a)

derive newtype instance functorReader :: Functor (Reader e)

type READER e r = (reader :: Reader e | r)

_reader :: Proxy "reader"
_reader = Proxy

liftReader :: forall e a r. Reader e a -> Run (READER e + r) a
liftReader = liftReaderAt _reader

liftReaderAt ::
  forall proxy symbol tail row e a
   . IsSymbol symbol
  => Row.Cons symbol (Reader e) tail row
  => proxy symbol
  -> Reader e a
  -> Run row a
liftReaderAt p r = Run.lift p r
-- proxyとReaderを渡す
-- Run.liftは proxy p とfunctor r を受け取るが、Readerは↑のderiveでFunctorになっているので渡すことができる
-- ここではpの値としてrが設定されたRunが返ってくる。こんな感じ Run (Free (VariantF (symbolの名前 :: 型))), symbolに紐づく値はr
-- Run (Free (VariantF r) a)

ask :: forall e r. Run (READER e + r) e
ask = askAt _reader -- proxyを渡してRunを返す

askAt ::
  forall proxy t e r s
  . IsSymbol s
  => Row.Cons s (Reader e) t r
  => proxy s
  -> Run r e
askAt sym = asksAt sym identity
{-
  呼び出しを追っていくと、これは↓のようになることがわかる。
    Run.lift sym (Reader identity)

  返ってくるのはこうなる
  Run (Free (VariantF (symの名前 :: 型))), symbolに紐づく値は(Reader identity)
-}

asks :: forall e r a. (e -> a) -> Run (READER e + r) a
asks = asksAt _reader

asksAt ::
  forall proxy t e r s a
  . IsSymbol s
  => Row.Cons s (Reader e) t r
  => proxy s
  -> (e -> a) -- この関数の引数の型は↑のReaderの型eと一致、更に返す型は↓のRunの型aと一致している
  -> Run r a -- 返すのはRun
asksAt sym f = liftReaderAt sym (Reader f) -- Readerは関数を持つのでfを渡せる

runReader ::
  forall e a r. 
  e -- 環境
  -> Run (READER e + r) a -- Run
  -> Run r a -- 新たなRun
runReader = runReaderAt _reader -- e と Run が渡される

runReaderAt ::
  forall proxy t e a r s
  . IsSymbol s -- Symbolである
  => Row.Cons s (Reader e) t r -- r は Symbol s の値として (Reader e) を持っていないといけない
  => proxy s -- Symbol s
  -> e       -- 環境 e
  -> Run r a -- Run r a
  -> Run t a
runReaderAt sym = loop
  where
  -- symがあったらLeft、なかったらRightになる
  handle = on sym Left Right
  -- eは環境(例えばint値や文字列など)、rはRunの中身なのでFree
  loop e r = case Run.peel r of
    -- LeftってことはFreeViewがBindだったってこと
    -- このときの r は Free (Bind CatList) みたいな形
    Left a -> case handle a of
      -- symがあったら
      Left (Reader k) ->
        trace (k e) \_ ->
        loop e (k e) -- kをeに適用した結果で再帰
      -- symがなかったら(これはRunの中身が合成されてる場合？readr + wirterみたいな)
      Right a' ->
        trace ("Left Right") \_ ->
        Run.send a' >>= runReaderAt sym e
    -- RightってことはFreeViewがReturnだったってこと
    -- このときの r は Free (Return CatList) みたいな形
    Right a ->
      trace (r) \_ ->
      pure a
