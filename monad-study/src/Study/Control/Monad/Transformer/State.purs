module Study.Control.Monad.Transformer.State where

import Prelude

import Data.Identity (Identity(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Newtype (class Newtype, unwrap)

-- State --
type State s = StateT s Identity

runState :: forall s a. State s a -> s -> Tuple a s
runState (StateT s) = unwrap <<< s

evalState :: forall s a. State s a -> s -> a
evalState (StateT m) s = case m s of 
  Identity (Tuple a _) -> a

execState :: forall s a. State s a -> s -> s
execState (StateT m) s = case m s of
  Identity (Tuple _ s') -> s'

mapState :: forall s a b. (Tuple a s -> Tuple b s) -> State s a -> State s b
mapState f = mapStateT (Identity <<< f <<< unwrap)

withState :: forall s a. (s -> s) -> State s a -> State s a
withState = withStateT
-- State --

-- Class --
class Monad m <= MonadState s m | m -> s where
  state :: forall a. (s -> (Tuple a s)) -> m a

get :: forall m s. MonadState s m => m s
get = state \s -> Tuple s s

put :: forall m s. MonadState s m => s -> m Unit
put s = state \_ -> Tuple unit s

gets :: forall s m a. MonadState s m => (s -> a) -> m a
gets f = state \s -> Tuple (f s) s

modify :: forall s m. MonadState s m => (s -> s) -> m s
modify f = state \s -> 
  let s' = f s
  in Tuple s' s'
-- Class --

-- Transformer --
newtype StateT s m a = StateT (s -> m (Tuple a s))

runStateT :: forall s m a. StateT s m a -> s -> m (Tuple a s)
runStateT (StateT s) = s

evalStateT :: forall s m a. Functor m => StateT s m a -> s -> m a
evalStateT (StateT m) s = fst <$> m s

execStateT :: forall s m a. Functor m => StateT s m a -> s -> m s
execStateT (StateT m) s = snd <$> m s

mapStateT :: forall s m1 m2 a b. (m1 (Tuple a s) -> m2 (Tuple b s)) -> StateT s m1 a -> StateT s m2 b
mapStateT f (StateT m) = StateT (f <<< m)

withStateT :: forall s m a. (s -> s) -> StateT s m a -> StateT s m a
withStateT f (StateT s) = StateT (s <<< f)


derive instance newtypeStateT :: Newtype (StateT s m a) _

instance functorStateT :: Functor m => Functor (StateT s m) where
  map f (StateT a) = StateT (\s -> map (\(Tuple b s') -> Tuple (f b) s') (a s))

instance applyStateT :: Monad m => Apply (StateT s m) where
  apply = ap

instance applicativeStateT :: Monad m => Applicative (StateT s m) where
  pure a = StateT \s -> pure $ Tuple a s

instance bindStateT :: Monad m => Bind (StateT s m) where
  bind (StateT x) f = StateT \s -> do
    Tuple v s' <- x s
    case f v of
      StateT st -> st s'

instance monadStateT :: Monad m => Monad (StateT s m)

instance monadStateStateT :: Monad m => MonadState s (StateT s m) where
  state f = StateT $ pure <<< f
-- Transformer --