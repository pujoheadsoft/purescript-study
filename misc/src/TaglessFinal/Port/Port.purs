module TaglessFinal.Port.Port where

import Prelude

import Control.Monad.Reader (ReaderT(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Class.Console (log)
import TaglessFinal.Domain.Article (Article)
import Type.Equality (class TypeEquals, to)

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
  --findByTitle title = xxx \f -> f.findByTitle title
  findByTitle = z1 _.findByTitle


class Monad m <= ArticlePresenterPort m where
  update :: (Array Article) -> m Unit

type ArticlePresenterFunction m r = {
  update :: (Array Article) -> m Unit
  | r
}

instance instanceArticlePresenterReaderT
  :: (Monad m, TypeEquals f (ArticlePresenterFunction m r))
  => ArticlePresenterPort (ReaderT f m) where
  update articles = ReaderT $ to >>> \f ->
    f.update articles

xxx :: forall r m a x. TypeEquals r x => (x -> m a) -> ReaderT r m a
xxx f = ReaderT $ to >>> f

z1 :: forall r m a x a1. TypeEquals r x => (x -> a1 -> m a) -> (a1 -> ReaderT r m a)
z1 f = \a1 -> ReaderT \r -> do
  let
    x = to r
    func = f x
  func a1

z2 :: forall r m a x a1 a2. TypeEquals r x => (x -> a1 -> a2 -> m a) -> (a1 -> a2 -> ReaderT r m a)
z2 f = \a1 a2 -> ReaderT \r -> do
  let
    x = to r
    func = f x
  func a1 a2

z3 :: forall r m a x a1 a2 a3. TypeEquals r x => (x -> a1 -> a2 -> a3 -> m a) -> (a1 -> a2 -> a3 -> ReaderT r m a)
z3 f = \a1 a2 a3 -> ReaderT \r -> do
  let
    x = to r
    func = f x
  func a1 a2 a3

abc :: forall r m a x y z. TypeEquals r x => (x -> y -> z -> m a) -> (y -> z -> ReaderT r m a)
abc f = \y z -> ReaderT \r -> do
  let
    xValue = to r
    func = f xValue
  func y z

class A args return | args -> return where
  get :: args -> return

instance instanceA3 :: A (a -> b -> c) (a -> b -> c -> String) where
  get _ = \_ _ _ -> "3"
else
instance instanceA2 :: A (a -> b) (a -> b -> String) where
  get _ = \_ _ -> "2"
else
instance instanceA1 :: A a (a -> String) where
  get _ = \_ -> "1"

class B args return | args -> return where
  reader :: args -> return

instance b2 :: B (r -> a1 -> a2 -> m a) (a1 -> a2 -> ReaderT r m a) where
  reader f = \a1 a2 -> do
    ReaderT $ \r -> do
      let func = f r
      func a1 a2
else
instance b1 :: B (r -> a1 -> m a) (a1 -> ReaderT r m a) where
  reader f = \a1 -> do
    ReaderT $ \r -> do
      let func = f r
      func a1

execB1 :: forall r m. Monad m => String -> ReaderT r m Unit
execB1 = reader (\r -> \a1 -> pure unit)

execB2 :: forall r m. Monad m => String -> String -> ReaderT r m Unit
execB2 = reader (\r -> \a1 a2 -> pure unit)

main :: Effect Unit
main = do
  log $ (get "") ""
  log $ (get (\_ -> "")) "" ""
  log $ (get (\_ _ -> "")) "" "" ""
  
-- class Moge r x fun return | r -> x, x -> fun, fun -> return where
--   moge :: TypeEquals r x => (x -> fun) -> return

-- instance moge2 :: Moge r x (a1 -> a2 -> m a) (a1 -> a2 -> ReaderT r m a) where
--   moge f = undefined
-- else
-- instance moge1 :: Moge r x (a1 -> m a) (a1 -> ReaderT r m a) where
--   moge f = undefined

-- else
-- instance moge3 :: Moge r m a x (a1 -> a2 -> a3) where
--   moge :: TypeEquals r x => (x -> (a1 -> a2 -> a3) -> m a) -> ((a1 -> a2 -> a3) -> ReaderT r m a)
--   moge f = undefined

  --  \a1 -> do
  --   ReaderT \r -> do
  --     let
  --       xValue = to r
  --       func = f xValue
  --     func a1

class Monad m <= T m where
  a1 :: String -> m Unit
  a2 :: String -> String -> m Unit
  -- a3 :: String -> String -> String -> m Unit

type TFunction m = {
  a1 :: String -> m Unit,
  a2 :: String -> String -> m Unit,
  a3 :: String -> String -> String -> m Unit
}

newtype TFunction2 m = TFunction2 {
  a1 :: String -> m Unit,
  a2 :: String -> String -> m Unit,
  a3 :: String -> String -> String -> m Unit
}
derive instance newtypeReaderT :: Newtype (TFunction2 m) _

instance instanceT2 :: Monad m => T (ReaderT (TFunction2 m) m) where
  a1 = reader (\(TFunction2 r) -> r.a1)
  a2 = reader (\(TFunction2 r) -> r.a2)
--  a3 t1 t2 t3 = xxx \f -> f.a3 t1 t2 t3

{-
  (a -> b) -> (b -> c)
  (a1 -> a2 -> (r -> m a))
-}

oge :: { a1 :: (String -> Unit) } -> (String -> Unit)
oge = _.a1
