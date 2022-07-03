module Study.Control.Monad.Transformer.Cont where

import Prelude

import Data.Identity (Identity(..))
import Data.Newtype (unwrap, class Newtype)

{-
  継続モナド変換子
  CPS(Continuation Passing Style) 継続渡しスタイルによる開発をサポートする
  継続 (次に行う処理) を関数で表して、それを引数に渡して実行することを「継続渡しスタイル」という
  (Schemeは継続を取り出すことができるようだ)
-}

-- Cont ----------------------------
type Cont r = ContT r Identity

-- | Contモナドを生成する
-- ContTの定義から c は(a -> m r)という関数。
-- またContTが持つ関数は m r を返さないといけないが、mはIdentityとしている。
-- unwrap <<< c で関数を合成しているが、これはモナドを剥がすことになる。
-- つまり (a -> r) となる。f は ((a -> r) -> r) なので、引数として適用できる。
-- これをIdentityで包む
cont :: forall a r. ((a -> r) -> r) -> Cont r a
cont f = ContT (\c -> Identity (f (unwrap <<< c)))

runCont :: forall r a. ContT r Identity a -> (a -> r) -> r
runCont cc k = unwrap (runContT cc (Identity <<< k))

mapCont :: forall r a. (r -> r) -> Cont r a -> Cont r a
mapCont f = mapContT (Identity <<< f <<< unwrap)

withCont :: forall a b r. ((b -> r) -> (a -> r)) -> Cont r a -> Cont r b
withCont f = withContT (compose Identity <<< f <<< compose unwrap)

-- Class ---------------------------

-- | call with current continuation をサポートするモナド
class Monad m <= MonadCont m where
  -- | callCCは「現在の継続」を呼び出し元が利用できるようにするもの。
  -- |
  -- | callCC は現在の継続(Current Continuation)を引数として関数を呼ぶ(それゆえCallCCという名がある)。
  -- | callCC を使うときの標準的なイディオムは，ラムダ式を与えて 継続に名前をつけるというもの。
  -- | そうすると、その名前のついた継続をスコープ内のあらゆる場所から呼ぶことで，それが入れ子の計算のどんなに深いところからでも脱出できる。
  -- | http://www.sampou.org/haskell/a-a-monads/html/contmonad.html
  callCC :: forall a. ((forall b. a -> m b) -> m a) -> m a

-- Transformer ---------------------

-- ContTは関数を持っている
-- 関数の引数は (a -> m r) という `関数` 
-- つまり関数を受け取って m r を返す関数
newtype ContT :: forall k. k -> (k -> Type) -> Type -> Type
newtype ContT r m a = ContT ((a -> m r) -> m r)

-- | 継続を指定して、ContTモナドで計算を実行する。
-- ContTの中身は ((a -> m r) -> m r) で、 kは (a -> m r) なのでそのまま渡せる
-- そしてContTが持ってる関数の返り値の型は m r なのでそのまま返せる
runContT :: forall r m a. ContT r m a -> (a -> m r) -> m r
runContT (ContT f) k = f k

-- | ContTモナドのアクションの基礎となるアクションを修正する。
mapContT :: forall r m a. (m r -> m r) -> ContT r m a -> ContT r m a
mapContT f (ContT m) = ContT (\k -> f (m k))

-- | ContTモナドのアクションで継続を修正する。
withContT :: forall r m a b. ((b -> m r) -> (a -> m r)) -> ContT r m a -> ContT r m b
withContT f (ContT m) = ContT (\k -> m (f k))


derive instance newtypeContT :: Newtype (ContT r m a) _

instance functorContT :: Functor m => Functor (ContT r m) where
  map f (ContT m) = ContT (\k -> m (\a -> k $ f a))

instance applyContT :: Apply m => Apply (ContT r m) where
  apply (ContT f) (ContT v) = ContT (\k -> f (\g -> v (\a -> k (g a))))

instance applicativeContT :: Applicative m => Applicative (ContT r m) where
  pure a = ContT (\k -> k a)

instance bindContT :: Bind m => Bind (ContT r m) where
  bind (ContT m) k = ContT (\k' -> m (\a -> case k a of ContT m' -> m' k'))

instance monadContT :: Monad m => Monad (ContT r m)

instance monadContContT :: Monad m => MonadCont (ContT r m) where
  callCC f = ContT (\k -> case f (\a -> ContT (\_ -> k a)) of ContT f' -> f' k)