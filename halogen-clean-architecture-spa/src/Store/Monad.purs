module Store.Monad where

import Prelude

import Control.Monad.Reader (class MonadAsk, ReaderT, ask, lift, runReaderT)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen (HalogenM, hoist)
import Halogen as H
import Store.Select (Selector(..))
import Halogen.Subscription (Emitter, Listener, makeEmitter)
import Halogen.Subscription as HS
import Unsafe.Coerce (unsafeCoerce)

-- MonadEffectが上位クラス(super class)
class MonadEffect m <= MonadStore a s m | m -> s a where
  getStore :: m s
  updateStore :: a -> m Unit
  emitSelected :: forall s'. Selector s s' -> m (Emitter s')

type HalogenStore action state =
  { value :: Ref state
  , emitter :: Emitter state
  , listener :: Listener state
  , reducer :: state -> action -> state
  }

newtype StoreT :: Type -> Type -> (Type -> Type) -> Type -> Type
newtype StoreT action state monad a = StoreT (ReaderT (HalogenStore action state) monad a)

derive newtype instance functorT :: Functor m => Functor (StoreT a s m)
derive newtype instance applyStoreT :: Apply m => Apply (StoreT a s m)
derive newtype instance applicativeStoreT :: Applicative m => Applicative (StoreT a s m)
derive newtype instance bindStoreT :: Bind m => Bind (StoreT a s m)
derive newtype instance monadStoreT :: Monad m => Monad (StoreT a s m)
derive newtype instance monadAskStoreT :: Monad m => MonadAsk (HalogenStore a s) (StoreT a s m)
derive newtype instance monadEffectStoreT :: MonadEffect m => MonadEffect (StoreT a s m)
derive newtype instance monadAffStoreT :: MonadAff m => MonadAff (StoreT a s m)

instance monadStoreStoreT :: MonadAff m => MonadStore a s (StoreT a s m) where
  getStore = StoreT do
    store <- ask
    liftEffect do
      Ref.read store.value
  
  updateStore action = StoreT do
    store <- ask
    liftEffect do
      current <- Ref.read store.value
      let newStore = store.reducer current action
      Ref.write newStore store.value
      HS.notify store.listener newStore
  
  emitSelected (Selector selector) = StoreT do
    store <- ask
    liftEffect do
      init <- Ref.read store.value
      prevRef <- Ref.new (selector.select init)
      pure $ filterEmitter store.emitter \new -> do
        prevDerived <- Ref.read prevRef
        let newDerived = selector.select new
        if selector.eq prevDerived newDerived then
          pure Nothing
        else do
          liftEffect $ Ref.write newDerived prevRef
          pure (Just newDerived)
    
    where
    filterEmitter :: forall x y. Emitter x -> (x -> Effect (Maybe y)) -> Emitter y
    filterEmitter emitter predicate = do
      let e = coerceEmitter emitter
      makeEmitter \k -> e \a -> predicate a >>= traverse_ k

      where
      coerceEmitter :: Emitter x -> ((x -> Effect Unit) -> Effect (Effect Unit))
      coerceEmitter = unsafeCoerce

instance monadStoreHalogenM :: MonadStore a s m => MonadStore a s (HalogenM st act slots out m) where
  getStore = lift getStore
  updateStore = lift <<< updateStore
  emitSelected = lift <<< emitSelected


runStoreT :: forall action store query input output monad
  . Monad monad
  => store
  -> (store -> action -> store)
  -> H.Component query input output (StoreT action store monad)
  -> Aff (H.Component query input output monad)
runStoreT initialStore reducer component = do
  -- HalogenStoreを生成
  halogenStore <- liftEffect do
    value <- Ref.new initialStore
    { emitter, listener } <- HS.create
    pure { value, emitter, listener, reducer }
  -- hoistはFree MonadをAffとして解釈し、runUIを実行できるようにする
  -- 第一引数はMonadの変換関数。渡されたComponentはこの変換関数が適用されたComponentになる。
  pure $ hoist (\(StoreT m) -> ((runReaderT m) halogenStore)) component
  
