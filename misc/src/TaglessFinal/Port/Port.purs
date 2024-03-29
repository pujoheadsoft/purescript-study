module TaglessFinal.Port.Port where

import Prelude

import Control.Monad.Reader (ReaderT)
import Data.ReaderTEtaConversionTransformer (readerT)
import TaglessFinal.Domain.Article (Article)
import Type.Equality (class TypeEquals)

{-
  Portの定義
  いくつかの特徴を持つ
  1.Tagless Final形式で型クラスが定義されている
  　これにより使う側(UseCase)は型クラスを合成して関数を呼び出すことができる

  2.ReaderTが各種型クラスのインスタンスとなっている
  　これはOrphan Instanceの問題を避けつつ定義と実装を切り離すために存在している。
  　どういうことかというと、本来Portとその実装はそれぞれ別のモジュールに定義したいのだが、
  　既存の型をインスタンスとする場合、Orphan Instanceと判断されてしまい別のモジュールに定義できないのだ。
  　それを回避するためにnewtypeで既存の型をラップする新たな型を定義して、そいつをインスタンスにする方法があるが、そいつをモナドとして使いたい場合
  　様々な型クラスのインスタンスを別途定義する必要があったり、ラップしたりアンラップしたり面倒という難点がある。
  　テストする場合にテスト用のインスタンスを定義するのもまた面倒だ。
  　
  　そこでインスタンスはReaderTにしておき、ReaderTは関数を持ったレコードを取得できるようにしておく。
  　そうすることによって、そのレコードの生成（実装部分）を外だしにできるというわけだ。
  　更にテストも容易になる。
-}
class Monad m <= ArticlePort m where
  findByTitle :: String -> m (Array Article)

-- 型クラスの関数と同じシグニチャの関数を持つレコード。あとで合成できるように拡張可能にしてある。
type ArticlePortFunction m r = {
  findByTitle :: String -> m (Array Article)
  | r
}

-- ReaderTをインスタンスとする
-- これにより関数を実行時に外側から自由に差し込める。
-- TypeEqualsによって関数が定義されていることを保証している。
instance instancePortReaderT
  :: (Monad m, TypeEquals f (ArticlePortFunction m r))
  => ArticlePort (ReaderT f m) where
  findByTitle = readerT _.findByTitle


class Monad m <= ArticlePresenterPort m where
  update :: (Array Article) -> m Unit

type ArticlePresenterFunction m r = {
  update :: (Array Article) -> m Unit
  | r
}

instance instanceArticlePresenterReaderT
  :: (Monad m, TypeEquals f (ArticlePresenterFunction m r))
  => ArticlePresenterPort (ReaderT f m) where
  update = readerT _.update

