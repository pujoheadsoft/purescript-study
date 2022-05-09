module Study.Control.Monad.Run.Reader
  ( Reader
  )
  where

import Prelude
import Data.Symbol (class IsSymbol)
import Data.Either (Either(..))
import Type.Proxy (Proxy(..))
import Prim.Row as Row
import Type.Row (type (+))
import Data.Functor.Variant (on)
import Study.Control.Monad.Run (Run)
import Study.Control.Monad.Run as Run

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

ask :: forall e r. Run (READER e + r) e
ask = askAt _reader

askAt ::
  forall proxy t e r s
  . IsSymbol s
  => Row.Cons s (Reader e) t r
  => proxy s
  -> Run r e
askAt sym = asksAt sym identity

asks :: forall e r a. (e -> a) -> Run (READER e + r) a
asks = asksAt _reader

asksAt ::
  forall proxy t e r s a
  . IsSymbol s
  => Row.Cons s (Reader e) t r
  => proxy s
  -> (e -> a)
  -> Run r a
asksAt sym f = liftReaderAt sym (Reader f)

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
  handle = on sym Left Right -- symがあったらLeft、なかったらRightになる
  loop e r = case Run.peel r of
    Left a -> case handle a of
      -- symがあったら
      Left (Reader k) ->
        loop e (k e) -- kをeに適用した結果で再帰
      -- symがなかったら
      Right a' ->
        Run.send a' >>= runReaderAt sym e
    Right a ->
      pure a

